open Printf

(* Type of a value *)
type _ t =
  | TVoid	: unit t
  | TInt	: int t
  | TFun	: ('r,'a) fn			-> ('r,'a) fn t 
(* Values / expressions *)
and _ x =
  | XInt	: int				-> int x
  | XId		: 'a t * id			-> 'a x
  | XAdd	: int x * int x			-> int x
  | XApp0	: ('r x,'r x) fn x		-> 'r x
  | XApp1	: ('r,'a x -> 'b) fn x * 'a x	-> ('r,'b) fn x
(* Function signature *)
and (_,_) fn =
  | FLam0	: 'r t				-> ('r x,'r x) fn
  | FLam1	: 'a t * id * ('r x,'b) fn	-> ('r x,'a x -> 'b) fn
(* Identifier *)
and id = string

let rec bind : type s r. (r,s) fn -> s -> r = fun fs f -> 
  match fs with
  | FLam0 _ -> f
  | FLam1 (t,n,fs') -> let x = XId (t,n) in bind fs' (f x)

(* let rec bind : *)
(*   type s r. (r,s) fn -> s -> r = *)
(*       fun fs f ->  *)
(*         match fs with *)
(*         | FVoid _ -> bind0 f *)
(*         | FLambda (t,n,fs') -> bind1 t n fs' f *)
(* and bind0 : *)
(*   type a. a x -> a x = *)
(*       fun f -> f *)
(* and bind1 : *)
(*   type a r s. a t -> string -> (r x, s) fn -> (a x -> s) -> r x =  *)
(*       fun t n fs f -> *)
(*         let x = XId (t,n) in bind fs (f x) *)

let (^^) f x = f x

let ret t = FLam0 t
let arg n t s = FLam1 (n, t, s)

let int = TInt
let a = arg
let r = ret
let s = (a int "a" ^^ a int "b" ^^ r int)

let _ = bind s (fun x y -> XAdd (x, y))
let _ = XApp0 (XApp1 (XApp1 (XId (TFun s, "foo"), XInt 0), XInt 0))

let rec mkcall : type s r. (r x,s) fn -> (r x,s) fn x -> s = fun fs fx ->
  match fs with
  | FLam0 t -> XApp0 fx
  | FLam1 (_,_,fs') -> fun x -> mkcall fs' (XApp1 (fx, x))

let _ = mkcall s (XId (TFun s, "foo"))

type env = binding list
and binding = EDefun : string * 'a x -> binding

let env : env ref = ref []
let add_defun : id -> 'a x -> unit = fun name x ->
  env := (EDefun (name, x)) :: !env

let defun : id -> ('r,'s) fn -> 's -> 's = fun name s impl ->
  add_defun name (bind s impl);
  mkcall s (XId (TFun s, name))

let foo = defun "foo" s (fun x y -> XAdd (x, y))
let _ = foo (XInt 0) (XInt 1)
let _ = env;;
  
  

(*
let rec mkcall : type s r. (r x,s) fn -> (r x,s) fn x -> s = fun fs fx ->
  match fs with
  | FVoid t -> mkcall0 fx
  | FLambda (_,_,fs') -> mkcall1 fs' fx
and mkcall0 : type r. (r x,r x) fn x -> r x = fun fx ->
  XApp0 fx
and mkcall1 : type a r s. (r x,s) fn -> (r x, a x -> s) fn x -> (a x -> s) =
  fun fs fx -> 
    fun x -> mkcall fs (XApp1 (fx, x))
*)

module type D =
  sig
    type t
    val name : string
  end

let newtype : string -> (module D) = fun name -> let module D = struct type t let name = name end in (module D)

module D1 =
  struct
    include (val newtype "HELLO")
  end

module D2 =
  struct
    include (val newtype "BAR")
  end

let _ = D1.x = D2.x

module M =
struct
  type t = private int
  let x : t = 0
end

module type T = sig type t end
module type F = functor (T:T) -> T

type header = string
type ident = string

class type ['d] env =
object('a)
  method requires : 'd env list
  method includes : header list

  method add : ident -> 'd -> unit
  method lookup : ident -> 'd option
end

class type env' = [int] env

let x : env' = object
  method add _ _ = ()
  method includes = []
  method requires = []
  method lookup _ = None
end;;

let _ = x#lookup

class type a =
object
  method foo : int
end

class type b =
object
  inherit a
end

module rec A :
  sig
    type t = B of B.t
  end
  = A
and B :
  sig
    type t = A of A.t
  end
  = B

module type M =
sig
  type t = (module T)
end

module rec M : sig end = 
  struct
    module M = M
  end

type nums = [ `ints | `floats ]

type _ t =
  | A : ([< nums] as 'a) -> 'a t

type _ x =
  | X : 'a t * 'a -> 'a x

let _ =
  X (A `ints, `ints);;


module type T = 
sig
  type s
end

module type M =
sig
  val make : 'a -> (module T with type s = 'a)
end
