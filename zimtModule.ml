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

	let rec mkcall : type s. s fn -> s fn x -> s = fun fs fx ->
	  match fs with
	  | FLam0 t -> XApp0 fx
	  | FLamV (_,fs') -> fun va -> mkcall fs' (XAppV (fx, va))
	  | FLam1 (_,_,fs') -> fun x -> mkcall fs' (XApp1 (fx, x))

        let bind env fsig fbody =
          let rec bind' : type s. env -> s fn -> s -> defvalue = fun e fs f -> 
	    match fs with
	    | FLam0 _ -> DefFunc (fsig, f)
	    | FLamV (n,fs') -> let va = VZero in bind' e fs' (f va)
	    | FLam1 (t,n,fs') -> let x = XId (t,(e,n)) in bind' e fs' (f x)
	  in
	  bind' env fsig fbody
      end

    let enum' n =
      let module E = (val ZimtEnum.make environment' n) in
      env#add_type n (DefType E.type');
      (module E : ENUM)

    let struct' n = 
      let module S = (val ZimtStruct.make environment' n) in
      env#add_type n (DefType S.type');
      (module S : STRUCT)

    let defvar' n t x =
      let e = env#env in
      env#add_value n (DefVar x);
      XId (t,(e,n))

    let defun' n fs fimpl =
      let e = env#env in
      let k = Fn.mkcall fs (XId (TFn fs, (e,n))) in
      let def = Fn.bind e fs fimpl in
      env#add_value n def;
      k

    let extern' n fs =
      let e = env#env in
      let k = Fn.mkcall fs (XId (TFn fs, (e,n))) in
      env#add_value n (DefExt fs);
      k
  end

let module' name includes = (module Make(struct
  let name = name
  let includes = includes
end) : MODULE)
