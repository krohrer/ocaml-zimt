open Zimt

module type ARGS =
  sig
    val name : Zimt.ident
    val includes : header list
  end

module Make (A : ARGS) : MODULE =
  struct
    module Env = ZimtEnv.Make(struct
      type dv = defvalue
      type dt = deftype
      let name = A.name
    end)

    let env = Env.make A.name

    let _ = List.iter env#add_include A.includes

    let name' = A.name
    let environment' = env

    module Fn =
      struct
	let (^^) f x = f x
	let ret t = FLam0 t
	let varargs n s = FLamV (n,s)
	let arg t n s = FLam1 (t,n,s)

	let rec mkcall : type s r. (r,s) fn -> (r,s) fn x -> s = fun fs fx ->
	  match fs with
	  | FLam0 t -> XApp0 fx
	  | FLamV (_,fs') -> fun va -> mkcall fs' (XAppV (fx, va))
	  | FLam1 (_,_,fs') -> fun x -> mkcall fs' (XApp1 (fx, x))
	
	let rec bind : type s r. env -> (r,s) fn -> s -> r = fun e fs f -> 
	  match fs with
	  | FLam0 _ -> f
	  | FLamV (n,fs') -> let va = VZero in bind e fs' (f va)
	  | FLam1 (t,n,fs') -> let x = XId (t,(e,n)) in bind e fs' (f x)
      end

    let enum' n =
      let module E = (val ZimtEnum.make environment' n) in
      env#add_type n (Type E.t');
      (module E : ENUM)

    let struct' n = 
      let module S = (val ZimtStruct.make environment' n) in
      env#add_type n (Type S.t');
      (module S : STRUCT)

    let defconst' n l =
      env#add_value n (ValConst l);
      XLit l

    let defun' n fs fimpl =
      let e = env#env in
      let k = Fn.mkcall fs (XId (TFn fs, (e,n))) in
      let impl = Fn.bind e fs fimpl in
      env#add_value n (ValFn (fs, impl));
      k

    let extern' n fs =
      let e = env#env in
      let k = Fn.mkcall fs (XId (TFn fs, (e,n))) in
      env#add_value n (ValEx fs);
      k
  end

let module' name includes = (module Make(struct
  let name = name
  let includes = includes
end) : MODULE)
