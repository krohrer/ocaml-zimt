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

class ['a] default_fold : ['a] fold_t
