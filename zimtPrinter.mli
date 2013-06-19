type pp = PrettyPrinter.t

(* val dump_t	: 'a Zimt.t		-> pp *)
(* val dump_x	: 'a Zimt.x	-> pp *)

(* val dump_forward	: 'a . 'a Zimt.t Lazy.t			-> pp *)
(* val dump_named		: 'a . 'a Zimt.t -> Zimt.q_ident	-> pp *)
(* val dump_ptr	        : 'a . 'a Zimt.ptr			-> pp *)
(* val dump_struct' 	: 'a . 'a Zimt.struct'			-> pp *)
(* val dump_enum		: 'a . 'a Zimt.enum			-> pp *)
(* val dump_prim		: 'a . 'a Zimt.prim			-> pp   *)
  
(* val dump_ptr		: 'a . 'a Zimt.ptr			-> pp *)
(* val dump_struct'	: 'a . 'a Zimt.struct'			-> pp *)
(* val dump_enum		: 'a . 'a Zimt.enum			-> pp *)
(* val dump_field		: 'a 'b . ('a,'b) Zimt.field		-> pp *)
(* val dump_prim		: 'a . 'a Zimt.prim			-> pp *)
(* val dump_fn		: 'a . 'a Zimt.fn			-> pp *)
(* val dump_varargs	: Zimt.varargs				-> pp *)

(* val dump_lit		: 'a . 'a Zimt.lit			-> pp *)
(* val dump_op1		: 'a 'b . ('a,'b) Zimt.op1		-> pp *)
(* val dump_op2		: 'a 'b 'c . ('a,'b,'c) Zimt.op2	-> pp *)
