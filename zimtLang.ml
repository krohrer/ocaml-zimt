type 'a t = 'a ZimtAST.t
type 'a x = 'a ZimtAST.x
type 'a lit = 'a ZimtAST.lit
type 'a ptr = 'a ZimtAST.ptr
type 'a struct' = 'a ZimtAST.struct'
type 'a enum = 'a ZimtAST.enum
type ('a,'b) field = ('a,'b) ZimtAST.field
type ('r,'a) fn = ('r,'a) ZimtAST.fn
type ident = ZimtAST.ident
type header = string

type global =
  | GlobalConst		: 'a lit		-> global
  | GlobalDefun		: ('r x,'s) fn * 'r x	-> global
  | GlobalExtern	: ('r,'s) fn		-> global

type type' =
  | Type	: 'a t				-> type'

open ZimtAST

module Fn =
struct
  let (^^) f x = f x
  let ret t = FLam0 t
  let arg t n s = FLam1 (t,n,s)

  let rec mkcall : type s r. (r,s) fn -> (r,s) fn x -> s = fun fs fx ->
    match fs with
    | FLam0 t -> XApp0 fx
    | FLam1 (_,_,fs') -> fun x -> mkcall fs' (XApp1 (fx, x))
      
  let rec bind : type s r. (r,s) fn -> s -> r = fun fs f -> 
    match fs with
    | FLam0 _ -> f
    | FLam1 (t,n,fs') -> let x = XId (t,n) in bind fs' (f x)
end

module Conditions =
  struct
    let raise_already_defined mn n =
      failwith (Printf.sprintf "%s: Already defined: %s" mn n)
  end

(* Define modules *)
module type ENV =
  sig
    type t
    val add : ident -> t -> unit
    val lookup : ident -> t option
  end

(* NAMED MODULE *)
module type NAMED =
  sig
    val name' : string
  end

(* CONCRETE TYPE *)
module type TYPE =
  sig
    include NAMED

    type w
    val t' : w t
  end

(* CONCRETE ENUM *)
module type ENUM =
  sig
    type e
    include TYPE with type w = e enum

    val case'	: ident -> int lit -> w x
  end

(* CONCRETE STRUCT *)
module type STRUCT =
  sig
    type s
    include TYPE with type w = s struct'

    val field'	: 'a t -> ident -> (w,'a) field
    val bits'	: int t -> ident -> int -> (w,int) field
    val pad'	: int t -> int -> unit
  end

(* MODULE TYPE *)
module type MODULE =
  sig
    include NAMED

    val enum : ident -> (module ENUM)
    val struct' : ident -> (module STRUCT)

    val defun : ident -> ('r x,'s) fn -> 's -> 's
    val extern : ident -> ('r x,'s) fn -> 's
  end

(* MODULE DESCRIPTION *)
module type MODULE_DESC =
  sig
    include NAMED

    type module' = (module MODULE)
    val requires : module' list
  end

(* Implementation *)

module MakeEnv (E : sig type t end) (N : NAMED) : ENV with type t = E.t =
  struct
    type t = E.t

    let bindings = Hashtbl.create 128

    let lookup n =
      try Some (Hashtbl.find bindings n) with
      | Not_found -> None

    let add n d =
      match lookup n with
      | None -> Hashtbl.add bindings n d
      | Some _ -> Conditions.raise_already_defined N.name' n
  end

module MakeGlobalEnv (N : NAMED) = MakeEnv (struct type t = global end) (N)
module MakeTypeEnv (N : NAMED) = MakeEnv (struct type t = type' end) (N)

module MakeStruct (E: ENV with type t = type') (N : NAMED) : STRUCT =
  struct
    module Fields = MakeTypeEnv(N)

    type s = unit
    type w = s struct'

    (* Incomplete type *)
    let t' = TStruct (SZero ())
    let name' = N.name'

    let repr : 'a struct' ref = ref (SZero t')
    let get_repr : unit -> 'a struct' = fun () -> !repr
    let set_repr : 'a struct' -> unit = fun r -> repr := r

    let field' : 'a t -> ident -> (w,'a) field = fun ft fn ->
      let f = FNamed (t', ft, fn) in
      Fields.add fn (Type ft);
      repr := SPlusField (!repr, (ft, fn));
      (* set_repr (SPlusField (get_repr (), (ft, fn))); *)
      f
    
    let bits' : int t -> ident -> int -> (w,'a) field = fun ft fn bc ->
      let f = FNamed (t', ft, fn) in
      Fields.add fn (Type ft);
      set_repr (SPlusBits (get_repr (), (ft, fn, bc)));
      f

    let pad' : int t -> int -> unit = fun ft bc ->
      set_repr (SPlusPadding (get_repr (), (ft, bc)))
  end

module MakeEnum (E: ENV with type t = global) (N: NAMED) : ENUM =
  struct
    type e = unit
    type w = e enum

    let t' = TEnum (EZero ())
    let name' = N.name'

    let repr : 'a enum ref = ref (EZero t')
    let get_repr : unit -> 'a enum = fun () -> !repr
    let set_repr : 'a enum -> unit = fun r -> repr := r

    let case' n l =
      E.add n (GlobalConst l);
      set_repr (EPlus (get_repr (), (n, l)));
      XId (t', n)
  end

module DefModule (D : MODULE_DESC) : MODULE =
  struct
    module Globals = MakeGlobalEnv(D)
    module Types = MakeTypeEnv(D)

    let name' = D.name'

    let enum n =
      let module N = struct let name' = n end in
       (module MakeEnum (Globals) (N) : ENUM)
    let struct' n = 
      let module D = struct
	let name' = n
	let requires = []
      end in
      (module MakeStruct (Types) (D) : STRUCT)

    let defun n fs fimpl =
      let k = Fn.mkcall fs (XId (TFun fs, n)) in
      let impl = Fn.bind fs fimpl in
      Globals.add n (GlobalDefun (fs, impl));
      k

    let extern n fs =
      let k = Fn.mkcall fs (XId (TFun fs, n)) in
      Globals.add n (GlobalExtern fs);
      k
  end

