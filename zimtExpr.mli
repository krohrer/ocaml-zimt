type 'a t = 'a Zimt.t
type 'a x = 'a Zimt.x
type 'a fn = 'a Zimt.fn
type 'a lit = 'a Zimt.lit
type varargs = Zimt.varargs
type q_ident = Zimt.q_ident
type ('a,'b) op1 = ('a,'b) Zimt.op1
type ('a,'b,'c) op2 = ('a,'b,'c) Zimt.op2
type ('a,'b) field = ('a,'b) Zimt.field

class type ['a] fold_t =
object
  method fold_quote	: 'b. 'b t * string			-> 'a->'a 
  method fold_lit	: 'b. 'b lit				-> 'a->'a
  method fold_id	: 'b. 'b t * q_ident			-> 'a->'a
  method fold_fn_id	: 'b. 'b fn * q_ident			-> 'a->'a
  method fold_let	: 'b 'c. 'b t * q_ident * 'b x * 'c x	-> 'a->'a
  method fold_arg1	: 'b 'c. 'b fn x * 'c x			-> 'a->'a
  method fold_va_call	: 'b. (varargs->'b x) fn x * varargs	-> 'a->'a
  method fold_arg1_call : 'b 'c. ('b x->'c x) fn x * 'b x	-> 'a->'a 
  method fold_varargs	: varargs				-> 'a->'a
  method fold_op1	: 'b 'c. ('b,'c) op1 * 'b x		-> 'a->'a
  method fold_op2	: 'b 'c 'd. ('b,'c,'d) op2 * 'b x * 'c x	-> 'a->'a
  method fold_field	: 'b 'c. ('b,'c) field			-> 'a->'a
  method fold_cond	: 'b. (Zimt.bool' x*'b x) list * 'b x	-> 'a->'a
  method fold_do	: 'b. Zimt.void' x list * 'b x		-> 'a->'a
  method fold		: 'b. 'b x				-> 'a->'a
end

class ['a] make_fold : 'a ZimtType.fold_t -> ['a] fold_t
