type 'a t = 'a ZimtAST.t
type 'a x = 'a ZimtAST.x
type ('r,'a) fn = ('r,'a) ZimtAST.fn
type ident = ZimtAST.ident

open ZimtAST

let void = TVoid
let int = TInt
let bool = TBool
let string = TString
let fn fs = TFun fs

let (^^) f x = f x
let returns t = FLam0 t
let param t n s = FLam1 (t,n,s)

let intL i = XLit (LInt i)
let boolL b = XLit (if b then LTrue else LFalse)
let stringL s = XLit (LString s)

let rec mkcall : type s r. (r,s) fn -> (r,s) fn x -> s = fun fs fx ->
  match fs with
  | FLam0 t -> XApp0 fx
  | FLam1 (_,_,fs') -> fun x -> mkcall fs' (XApp1 (fx, x))

let rec bind : type s r. (r,s) fn -> s -> r = fun fs f -> 
  match fs with
  | FLam0 _ -> f
  | FLam1 (t,n,fs') -> let x = XId (t,n) in bind fs' (f x)


type header = string

module Conditions =
  struct
    let raise_already_defined mn n =
      failwith (Printf.sprintf "%s: Already defined: %s" mn n)
  end

module type MODULE_DESC =
  sig
    val name : ident
    val headers : header list
  end

module Definition =
  struct
     type t = 
      | EDefun : ('r x,'s) fn * 'r x -> t
      | EExtern : ('r x,'s) fn -> t
  end

module Environment (D : MODULE_DESC) =
  struct
    type t = (ident, Definition.t) Hashtbl.t

    let defs = Hashtbl.create 128

    let lookup n =
      try Some (Hashtbl.find defs n) with
      | Not_found -> None

    let add n d =
      match lookup n with
      | None -> Hashtbl.add defs n d
      | Some _ -> Conditions.raise_already_defined D.name n
  end

module Module (D : MODULE_DESC) =
  struct
    module Desc = D
    module Env = Environment(D)

    let extern n fs =
      let k = mkcall fs (XId (TFun fs, n)) in
      Env.add n (Definition.EExtern fs);
      k

    let defun n fs fimpl =
      let k = mkcall fs (XId (TFun fs, n)) in
      let impl = bind fs fimpl in
      Env.add n (Definition.EDefun (fs, impl));
      k

    let write_ocaml_interface out = failwith "NYI"
    let write_ocaml_impl out = failwith "NYI"
    let write_c_header out = failwith "NYI"
    let write_c_impl out = failwith "NYI"
  end
