open ZimtAST

(* Function signature DSL and helpers *)
(* module Fn = *)
(* struct *)
(*   let (^^) f x = f x *)
(*   let ret t = FLam0 t *)
(*   let varargs n s = FLamV (n,s) *)
(*   let arg t n s = FLam1 (t,n,s) *)

(*   let rec mkcall : type s r. (r,s) fn -> (r,s) fn x -> s = fun fs fx -> *)
(*     match fs with *)
(*     | FLam0 t -> XApp0 fx *)
(*     | FLamV (_,fs') -> fun va -> mkcall fs' (XAppV (fx, va)) *)
(*     | FLam1 (_,_,fs') -> fun x -> mkcall fs' (XApp1 (fx, x)) *)
      
(*   let rec bind : type s r. env -> (r,s) fn -> s -> r = fun e fs f ->  *)
(*     match fs with *)
(*     | FLam0 _ -> f *)
(*     | FLamV (n,fs') -> let va = VZero in bind e fs' (f va) *)
(*     | FLam1 (t,n,fs') -> let x = XId (t,(e,n)) in bind e fs' (f x) *)
(* end *)

(* (\* Exceptional conditions *\) *)
(* module Conditions = *)
(*   struct *)
(*     let already_defined mn n = *)
(*       failwith (Printf.sprintf "%s: Already defined: %s" mn n) *)
(*   end *)

(* (\* Environments *\) *)
(* module HeaderSet = Set.Make(struct *)
(*   type t = header *)
(*   let compare h1 h2 = compare h1 h2 *)
(* end) *)

(* module EnvSet = Set.Make(struct *)
(*   type t = env *)
(*   let compare e1 e2 = compare e1 e2 (\* compare (Oo.id e1) (Oo.id e2) *\) *)
(* end) *)

(* let make_env name = *)
(* object(self : mutenv) *)
(*   val types = Hashtbl.create 128 *)
(*   val values = Hashtbl.create 128 *)
(*   val mutable includes = HeaderSet.empty *)
(*   val mutable requires = EnvSet.empty *)

(*   method env = (self :> env) *)

(*   method includes : header list = HeaderSet.elements includes *)

(*   method requires : env list = EnvSet.elements requires *)

(*   method add_include h = *)
(*     includes <- HeaderSet.add h includes *)

(*   method lookup_value n = *)
(*     try Some (Hashtbl.find values n) with Not_found -> None *)

(*   method lookup_type n = *)
(*     try Some (Hashtbl.find types n) with Not_found -> None *)

(*   method add_value n v = *)
(*     match self#lookup_value n with *)
(*     | None -> *)
(* 	      Hashtbl.add values n v *)
(* 	      (\* failwith "TODO: Add requires from values" *\) *)
(*     | Some _ -> *)
(* 	      Conditions.already_defined name n *)

(*   method add_type n t = *)
(*     match self#lookup_type n with *)
(*     | None -> *)
(* 	      Hashtbl.add types n t *)
(* 	      (\* failwith "TODO: Add requires from values" *\) *)
(*     | Some _ -> *)
(* 	      Conditions.already_defined name n *)
(* end *)

(* module Structure = *)
(*   struct *)
(*     module type ARGS = *)
(*       sig *)
(* 	val name'		: ident *)
(* 	val environment'	: mutenv *)
(*       end *)

(*     module type S = STRUCT *)

(*     module Make (A : ARGS) : S = *)
(*       struct *)
(* 	let env = M.environment' *)

(* 	type s = unit *)
(* 	type w = s struct' *)

(* 	let repr : 'a struct' ref = ref (SZero ()) *)

(* 	let t' = TForward (lazy (TStruct !repr)) *)
(* 	let name' = N.name' *)

(* 	let field' : 'a t -> ident -> (w,'a) field = fun ft fn -> *)
(* 	  let f = FNamed (t', ft, fn) in *)
(* 	  env#add_type fn (Type ft); *)
(* 	  repr := SPlusField (!repr, (ft, fn)); *)
(* 	  f *)
	    
(* 	let bits' : int t -> ident -> int -> (w,'a) field = fun ft fn bc -> *)
(* 	  let f = FNamed (t', ft, fn) in *)
(* 	  env#add_type fn (Type ft); *)
(* 	  repr := SPlusBits (!repr, (ft, fn, bc)); *)
(* 	  f *)

(* 	let pad' : int t -> int -> unit = fun ft bc -> *)
(* 	  repr := SPlusPadding (!repr, (ft, bc)) *)
(*       end *)
(* end *)
	
(* module MakeStruct (D : STRUCT_DESC) : STRUCT = *)
(*   struct *)
(*     let env = M.environment' *)

(*     type s = unit *)
(*     type w = s struct' *)

(*     let repr : 'a struct' ref = ref (SZero ()) *)

(*     let t' = TForward (lazy (TStruct !repr)) *)
(*     let name' = N.name' *)

(*     let field' : 'a t -> ident -> (w,'a) field = fun ft fn -> *)
(*       let f = FNamed (t', ft, fn) in *)
(*       env#add_type fn (Type ft); *)
(*       repr := SPlusField (!repr, (ft, fn)); *)
(*       f *)
    
(*     let bits' : int t -> ident -> int -> (w,'a) field = fun ft fn bc -> *)
(*       let f = FNamed (t', ft, fn) in *)
(*       env#add_type fn (Type ft); *)
(*       repr := SPlusBits (!repr, (ft, fn, bc)); *)
(*       f *)

(*     let pad' : int t -> int -> unit = fun ft bc -> *)
(*       repr := SPlusPadding (!repr, (ft, bc)) *)
(*   end *)

(* module MakeEnum (D : ENUM_DESC) : ENUM = *)
(*   struct *)
(*     let env = M.environment' *)

(*     type e = unit *)
(*     type w = e enum *)

(*     (\* Incomplete type *\) *)
(*     let t' = TEnum (EZero ()) *)
(*     let name' = M.name' *)

(*     let repr : 'a enum ref = ref (EZero t') *)

(*     let case' n l = *)
(*       env#add_value n (ValConst l); *)
(*       repr := EPlus (!repr, (n,l)); *)
(*       XId (t', (env#env,n)) *)
(*   end *)

module type ARGS =
  sig
    val name : string
  end

module Make (A : ARGS) : MODULE =
  struct
    let env = ZimtEnv.make A.name

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

    let include' h =
      env#add_include h

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

let module' name = (module Make(struct
  let name = name
end) : MODULE)
