class type ['a] fold_t =
object
  method fold_caml	: 'b. 'b Caml.t			-> 'a -> 'a
  method fold_forward	: 'b. 'b Zimt.t Lazy.t		-> 'a -> 'a
  method fold_named	: 'b. 'b Zimt.t * Zimt.q_ident	-> 'a -> 'a
  method fold_ptr	: 'b. 'b Zimt.ptr		-> 'a -> 'a
  method fold_struct	: 'b. 'b Zimt.struct'		-> 'a -> 'a
  method fold_enum	: 'b. 'b Zimt.enum		-> 'a -> 'a
  method fold_prim	: 'b. 'b Zimt.prim		-> 'a -> 'a
  method fold_fn	: 'b. 'b Zimt.fn		-> 'a -> 'a
  method fold		: 'b. 'b Zimt.t			-> 'a -> 'a
end

class ['a] default_fold =
object(self)
  (* TODO : check if fold_right? *)
  method fold_caml : type b. b Caml.t -> 'a -> 'a = fun v a ->
    (* Right fold *)
    a
      
  method fold_forward : type b. b Zimt.t Lazy.t -> 'a -> 'a = fun (lazy t) a ->
    (* Right fold *)
    self#fold t a

  method fold_named : type b. b Zimt.t * Zimt.q_ident -> 'a -> 'a = fun (t,_) a ->
    (* Right fold *)
    self#fold t a

  method fold_ptr : type b. b Zimt.ptr -> 'a -> 'a = fun pt a ->
    (* Right fold *)
    match pt with
    | Zimt.PHeap t          -> self#fold t a
    | Zimt.PStatic t        -> self#fold t a
    | Zimt.PFn ft           -> self#fold_fn ft a

  method fold_struct : type b. b Zimt.struct' -> 'a -> 'a = fun st a ->
    (* Right fold? *)
    match st with
    | Zimt.SZero _ -> a
    | Zimt.SPlusField (s,(t,_)) -> self#fold t (self#fold_struct s a)
    | Zimt.SPlusBits (s,(t,_,_)) -> self#fold t (self#fold_struct s a)
    | Zimt.SPlusPadding (s,(t,_)) -> self#fold t (self#fold_struct s a)

  method fold_enum : type b. b Zimt.enum -> 'a -> 'a = fun _ a ->
    (* Right fold *)
    a

  method fold_prim : type b. b Zimt.prim -> 'a -> 'a  = fun _ a ->
    (* Right fold *)
    a

  method fold_fn : type s. s Zimt.fn -> 'a -> 'a = fun s a ->
    (* Right fold? *)
    match s with
    | Zimt.FnArgRet (t,_,rt)        -> self#fold t (self#fold rt a)
    | Zimt.FnVARet (_,rt)           -> self#fold rt a
    | Zimt.FnArg (t,_,s)            -> self#fold t (self#fold_fn s a)

  method fold : type b. b Zimt.t -> 'a -> 'a = fun t a ->
    (* Right fold *)
    match t with
    | Zimt.TCaml vt         -> self#fold_caml vt a
    | Zimt.TForward lt      -> self#fold_forward lt a
    | Zimt.TNamed (r,q)     -> self#fold_named (r,q) a
    | Zimt.TPtr pt          -> self#fold_ptr pt a
    | Zimt.TStruct st       -> self#fold_struct st a
    | Zimt.TEnum et         -> self#fold_enum et a
    | Zimt.TPrim pt         -> self#fold_prim pt a
end

