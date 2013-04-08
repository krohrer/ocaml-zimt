open Zimt

class type ['a] fold_t =
object
  method fold_caml	: 'b. 'b Caml.t		-> 'a -> 'a
  method fold_forward	: 'b. 'b t Lazy.t	-> 'a -> 'a
  method fold_named	: 'b. 'b t * q_ident	-> 'a -> 'a
  method fold_ptr	: 'b. 'b ptr		-> 'a -> 'a
  method fold_struct	: 'b. 'b struct'	-> 'a -> 'a
  method fold_enum	: 'b. 'b enum		-> 'a -> 'a
  method fold_prim	: 'b. 'b prim		-> 'a -> 'a
  method fold_fn	: 'b. 'b fn		-> 'a -> 'a
  method fold		: 'b. 'b t		-> 'a -> 'a
end

class ['a] default_fold =
object(self)
  (* TODO : check if fold_right? *)
  method fold_caml : type b. b Caml.t -> 'a -> 'a = fun v a ->
    (* Right fold *)
    a
      
  method fold_forward : type b. b t Lazy.t -> 'a -> 'a = fun (lazy t) a ->
    (* Right fold *)
    self#fold t a

  method fold_named : type b. b t * q_ident -> 'a -> 'a = fun (t,_) a ->
    (* Right fold *)
    self#fold t a

  method fold_ptr : type b. b ptr -> 'a -> 'a = fun pt a ->
    (* Right fold *)
    match pt with
    | PHeap t          -> self#fold t a
    | PStatic t        -> self#fold t a
    | PFn ft           -> self#fold_fn ft a

  method fold_struct : type b. b struct' -> 'a -> 'a = fun st a ->
    (* Right fold? *)
    match st with
    | SZero _			-> a
    | SPlusField (s,(t,_))	-> self#fold t (self#fold_struct s a)
    | SPlusBits (s,(t,_,_))	-> self#fold t (self#fold_struct s a)
    | SPlusPadding (s,(t,_))	-> self#fold t (self#fold_struct s a)

  method fold_enum : type b. b enum -> 'a -> 'a = fun _ a ->
    (* Right fold *)
    a

  method fold_prim : type b. b prim -> 'a -> 'a  = fun _ a ->
    (* Right fold *)
    a

  method fold_fn : type s. s fn -> 'a -> 'a = fun s a ->
    (* Right fold? *)
    match s with
    | FnArgRet (t,_,rt)        -> self#fold t (self#fold rt a)
    | FnVARet (_,rt)           -> self#fold rt a
    | FnArg (t,_,s)            -> self#fold t (self#fold_fn s a)

  method fold : type b. b t -> 'a -> 'a = fun t a ->
    (* Right fold *)
    match t with
    | TCaml vt         -> self#fold_caml vt a
    | TForward lt      -> self#fold_forward lt a
    | TNamed (r,q)     -> self#fold_named (r,q) a
    | TPtr pt          -> self#fold_ptr pt a
    | TStruct st       -> self#fold_struct st a
    | TEnum et         -> self#fold_enum et a
    | TPrim pt         -> self#fold_prim pt a
end

(* module Make (A : sig *)
(*   type w *)
(*   val w'	: w *)
(*   val name	: ident *)
(*   val env	: mutenv *)
(*   val requires	: header list *)
(* end) : TYPE with type w = A.w = *)
(*   struct *)
(*     let env' = A.env#env *)
(*     let name' = A.name *)
(*     let requires' = A.requires *)

(*     type w = A.w *)
(*     let t' =  *)
(*   end *)
