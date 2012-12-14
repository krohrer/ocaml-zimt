(** Type witnesses / phantom types for builtin types *)

(** void' is not a polymorphic variant on purpose, so it cannot be unified with
scalar types *)
type void' = unit

(** Scalar types *)
type int'	= [ `Int ]
type bool'	= [ `Bool ]
type float'	= [ `Float64 ]

type natint'    = [ `NatInt ]

type int8'	= [ `Int8 ]
type int16'	= [ `Int16 ]
type int32'	= [ `Int32 ]
type int64'	= [ `Int64 ]

type uint8'	= [ `UInt8 ]
type uint16'	= [ `UInt16 ]
type uint32'	= [ `UInt32 ]
type uint64'	= [ `UInt64 ]

type float16'	= [ `Float16 ]
type float32'	= [ `Float32 ]
type float64'	= [ `Float64 ]

(** Builtin type families *)
type sints'	= [ `Int8 | `Int16 | `Int32 | `Int64 | `NatInt ]
type uints'	= [ `UInt8 | `UInt16 | `UInt32 | `UInt64 ]
type floats'	= [ `Float16 | `Float32 | `Float64 ]
type ints'	= [ sints' | uints' ]
type nums'	= [ ints' | floats' ]
(** Builtin composite types *)
type 'a ptr'

type 'a struct'
type ('a,'b) field

type 'a const'
type string'

(* C Language description hoisted into OCaml (with some new
constructs), using type witnesses and GADTs for added compile time
safety. (why write a typechecker when you can use OCaml's?) *)

(** Type of expressions *)
type 'a t

(** Identifiers *)
and ident = string

(** Expressions *)
and _ x =
  (** Literals *)
  | XLit	: 'a lit				-> 'a x
  (** Identifiers *)
  | XId		: 'a t * ident				-> 'a x
  (** New bindings *)
  | XLet	: 'a t * ident * 'a x * ('a x -> 'b x)	-> 'b x
  (** Function application, base case *)
  | XApp0	: ('r x,'r x) fn x			-> 'r x
  (** Function application, recursive case *)
  | XApp1	: ('r x,'a x -> 'b) fn x * 'a x		-> 'r x
  (** Unary operators *)
  | XOp1	: ('a,'b) op1 * 'a x			-> 'b x
  (** Binary operators *)
  | XOp2	: ('a,'b,'c) op2 * 'a x * 'b x		-> 'c x
  (** Conditional expression (why if if you can have cond?) *)
  | XCond	: (bool' x * 'a x) list * 'a x		-> 'a x
  (** Explicity sequencing of expressions *)
  | XDo		: void' x list * 'a x			-> 'a x
  (* TOOD : Add looping construct *)
  (* TODO : Add setter *)


(** Function signature *)
and (_,_) fn =
  | FLam0	: 'r t				-> ('r x,'r x) fn
  | FLam1	: 'a t * ident * ('r x,'b) fn	-> ('r x,'a x -> 'b) fn


(** Literals *)
and _ lit = 
  | LInt	: int		-> int' lit
  | LFloat	: float		-> float' lit
  | LStr	: string	-> string' lit


(** Unary operators *)
and (_,_) op1 =
  | O1Arith	: 'a arith1		-> ('a,'a) op1
  | O1Bit	: [`Not]		-> ([<ints'] as 'a,'a) op1
  | O1Logic	: [`Not]		-> (bool',bool') op1
  | O1Cast	: 'a t			-> ('a,'b) op1
  | O1PGet	: 			   ('a ptr','a) op1
  | O1SGet	: ('a,'b) field		-> ('a    ,'b) op1


(** Binary operators *)
and (_,_,_) op2 =
  | O2Arith	: 'a arith2			-> ('a,'a,'a) op2
  | O2PArith	: [`Add|`Sub]			-> ('a ptr',[<ints'],'a ptr') op2
  | O2Comp	: [`Eq|`NE|`Gt|`Lt|`GE|`LE]	-> ([<nums'|bool'] as 'a,'a,'a) op2
  | O2Logic	: [`And|`Or]			-> (bool',bool',bool') op2
  | O2Bit	: [`And|`Or|`Xor|`Shl|`Shr]	-> ([<ints'] as 'a,'a,'a) op2
  | O2PSet	: 				   ('a ptr','a,'a) op2
  | O2SSet	: ('a,'b) field			-> ('a struct','a,'a) op2


(** Unary arithmetic op *)
and _ arith1 =
  | A1Neg	: [<nums'] arith1
  | A1PreInc	: [<ints'] arith1
  | A1PreDec	: [<ints'] arith1
  | A1PostInc	: [<ints'] arith1
  | A1PostDec	: [<ints'] arith1


(** Binary arithmetic op *)
and _ arith2 =
  | A2Add	: [<nums'] arith2
  | A2Sub	: [<nums'] arith2
  | A2Mul	: [<nums'] arith2
  | A2Div	: [<nums'] arith2
  | A2Mod	: [<ints'] arith2
