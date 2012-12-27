open Zimt

class ['a] fold =
object(self)
  method fold_caml : type b. b Caml.t -> 'a -> 'a = fun v a ->
    a

  method fold_forward : type b. b t Lazy.t -> 'a -> 'a = fun (lazy t) a ->
    self#fold t a

  method fold_named : type b. b * q_ident -> 'a -> 'a = fun _ a ->
    a

  method fold_ptr : type b. b ptr -> 'a -> 'a = fun pt a ->
    match pt with
    | PHeap t	-> self#fold t a
    | PStatic t	-> self#fold t a
    | PFn ft	-> self#fold_fn ft a

  method fold_struct : type b. b struct' -> 'a -> 'a = fun st a ->
    match st with
    | SZero _ -> a
    | SPlusField (s,(t,_)) -> self#fold t (self#fold_struct s a)
    | SPlusBits (s,(t,_,_)) -> self#fold t (self#fold_struct s a)
    | SPlusPadding (s,(t,_)) -> self#fold t (self#fold_struct s a)

  method fold_enum : type b. b enum -> 'a -> 'a = fun _ a ->
    a

  method fold_prim : type b. b prim -> 'a -> 'a  = fun _ a ->
    a

  method fold_fn : type s. s fn -> 'a -> 'a = fun s a ->
    match s with
    | FnRet t -> self#fold t a
    | FnVarArgs (_,s) -> self#fold_fn s a
    | FnArg (t,_,s) -> self#fold t (self#fold_fn s a)

  method fold : type b. b t -> 'a -> 'a = fun t a ->
    match t with
    | TCaml vt		-> self#fold_caml vt a
    | TForward lt	-> self#fold_forward lt a
    | TNamed (r,q)	-> self#fold_named (r,q) a
    | TPtr pt		-> self#fold_ptr pt a
    | TStruct st	-> self#fold_struct st a
    | TEnum et		-> self#fold_enum et a
    | TPrim pt		-> self#fold_prim pt a
end
