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

class ['a]  make_fold (type_fold:'a ZimtType.fold_t) =
object(self)
  method fold_quote
    : type b. b t * string -> 'a->'a
      = fun (t,_) a ->
	type_fold#fold t a

  method fold_lit
    : type b. b lit -> 'a->'a
      = fun _ a -> a
    
  method fold_id
    : type b. b t * q_ident -> 'a->'a
    = fun (t,_) a ->
      type_fold#fold t a

  method fold_fn_id
    : type b. b fn * q_ident -> 'a->'a
      = fun (fs,_) a ->
	type_fold#fold_fn fs a

  method fold_let
    : type b c. b t * q_ident * b x * c x -> 'a->'a
      = fun (t,_,init,body) a ->
	type_fold#fold t (self#fold init (self#fold body a))

  method fold_arg1
    : type b c. b fn x * c x -> 'a->'a
      = fun (fx,x) a ->
	self#fold fx (self#fold x a)

  method fold_va_call
    : type b. (varargs->b x) fn x * varargs -> 'a->'a
      = fun (fx,va) a ->
	self#fold fx (self#fold_varargs va a)

  method fold_varargs
    : varargs -> 'a ->' a
      = fun va a ->
	match va with
	| Zimt.VZero		-> a
	| Zimt.VPlus (x,va')	-> self#fold x (self#fold_varargs va' a)
	
  method fold_arg1_call
    : type b c. (b x->c x) fn x * b x -> 'a->'a
      = fun (fx,x) a ->
	self#fold fx (self#fold x a)

  method fold_op1
    : type b c. (b,c) op1 * b x -> 'a->'a
      = fun (o,x) a ->
	let a = self#fold x a in
	match o with
	| Zimt.O1SGet f -> self#fold_field f a
	| _ -> a

  method fold_field
    : type b c. (b,c) field  -> 'a->'a
      = fun f a ->
	match f with
	| Zimt.FDeref p		-> type_fold#fold_ptr p a
	| Zimt.FSubscript (p,x) -> type_fold#fold_ptr p (self#fold x a)
	| Zimt.FNamed (t,ft,_)	-> type_fold#fold t (type_fold#fold ft a)
	
  method fold_op2
    : type b c d. (b,c,d) op2 * b x * c x -> 'a->'a
      = fun (o,lx,rx) a ->
	let a = self#fold lx (self#fold rx a) in
	match o with
	| Zimt.O2SSet f -> self#fold_field f a
	| _ -> a

  method fold_cond
    : type b. (Zimt.bool' x*b x) list * b x -> 'a->'a
      = fun (cases,xelse) a ->
	let fold_case (xpred,xthen) a =
	  self#fold xpred (self#fold xthen a)
	in
	List.fold_right fold_case cases (self#fold xelse a)

  method fold_do
    : type b. Zimt.void' x list * b x -> 'a->'a
      = fun (xs,x) a ->
	List.fold_right self#fold xs (self#fold x a)

  method fold
    : type b. b x -> 'a->'a
      = fun x a ->
	match x with 
	| Zimt.XQuote (q,s)		-> self#fold_quote (q,s) a
	| Zimt.XLit l			-> self#fold_lit l a
	| Zimt.XId (t,n)		-> self#fold_id (t,n) a
	| Zimt.XFnId (fs,n)		-> self#fold_fn_id (fs,n) a
	| Zimt.XLet (t,n,i,b)		-> self#fold_let (t,n,i,b) a
	| Zimt.XFnArg1 (fx,x)		-> self#fold_arg1 (fx,x) a
	| Zimt.XFnVACall (fx,va)	-> self#fold_va_call (fx,va) a
	| Zimt.XFnArg1Call (fx,x)	-> self#fold_arg1_call (fx,x) a
	| Zimt.XOp1 (o,x)		-> self#fold_op1 (o,x) a
	| Zimt.XOp2 (o,x,y)		-> self#fold_op2 (o,x,y) a
	| Zimt.XCond (cases,xelse)	-> self#fold_cond (cases,xelse) a
	| Zimt.XDo (xs,x)		-> self#fold_do (xs,x) a
end
