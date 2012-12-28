open Zimt

let failwithf fmt = Printf.ksprintf failwith fmt

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
	let arg_ret (t,n) rt = FnArgRet (t,Some n,rt)
	let uarg_ret t rt = FnArgRet (t,None,rt)
	let varargs_ret n s = FnVARet (n,s)
	let arg (t,n) s = FnArg (t,Some n,s)
	let uarg t s = FnArg (t,None,s)

	let rec mkcall : type s. s fn -> s fn x -> s = fun fs fx ->
	  match fs with
	  | FnArgRet (_,_,_) -> fun x -> XFnArg1Call (fx,x)
	  | FnVARet (_,_) -> fun va -> XFnVACall (fx, va)
	  | FnArg (_,_,fs') -> fun x -> mkcall fs' (XFnArg1 (fx, x))

	let apply = function
	  | XFnId (fn,_) as fx -> mkcall fn fx
	  | _ -> failwithf "Zimt.Fn.apply: TODO"

        let bind env fsig fbody =
          let rec bind' : type s. env -> s fn -> s -> defvalue = fun e fs f -> 
	    match fs with
	    | FnArgRet (t,nopt,rt) -> failwith "TODO"
	    | FnVARet (n,rt) ->failwith "TODO"
	    | FnArg (t,nopt,rt) -> failwith "TOOD"
	  (*
	    | FnRet _ -> DefFunc (fsig, f)
	    | FnVarArgs (n,fs') -> let va = VZero in bind' e fs' (f va)
	    | FnArg (t,Some n,fs') -> let x = XId (t,(e,n)) in bind' e fs' (f x)
	    | FnArg (t,None,fs') -> let x = XId (t,(e,"_")) in bind' e fs' (f x)
	  *)
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
      let f = XFnId (fs, (e,n)) in
      let k = Fn.mkcall fs f in
      let def = Fn.bind e fs fimpl in
      env#add_value n def;
      k, XOp1 (O1FnRef, f)

    let extern' n fs =
      let e = env#env in
      let f = XFnId (fs, (e,n)) in
      let k = Fn.mkcall fs f in
      env#add_value n (DefExt fs);
      k, XOp1 (O1FnRef, f)
  end

let module' name includes = (module Make(struct
  let name = name
  let includes = includes
end) : MODULE)
