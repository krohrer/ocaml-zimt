open Zimt

class ['a] fold =
object(self)
  method fold_value : type b. b value -> 'a -> 'a = fun t a ->
    self#fold t.value_type a

  method fold_forward : type b. b t Lazy.t -> 'a -> 'a = fun (lazy t) a ->
    self#fold t a

  method fold_named : type b. b * q_ident -> 'a -> 'a = fun _ a ->
    a

  method fold_ptr : type b. b ptr -> 'a -> 'a = fun pt a ->
    match pt with
    | PHeap t -> self#fold t a
    | PStatic t -> self#fold t a

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

  method fold_fn : type r s. (r x,s) fn -> 'a -> 'a = fun s a ->
    match s with
    | FLam0 t -> self#fold t a
    | FLamV (_,s) -> self#fold_fn s a
    | FLam1 (t,_,s) -> self#fold t (self#fold_fn s a)

  method fold : type b. b t -> 'a -> 'a = fun t a ->
    match t with
    | TValue vt		-> self#fold_value vt a
    | TForward lt	-> self#fold_forward lt a
    | TNamed (r,q)	-> self#fold_named (r,q) a
    | TPtr pt		-> self#fold_ptr pt a
    | TStruct st	-> self#fold_struct st a
    | TEnum et		-> self#fold_enum et a
    | TPrim pt		-> self#fold_prim pt a
    | TFn ft		-> self#fold_fn ft a
end
