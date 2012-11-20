(* Type witnesses for builtin types *)

(* void' is not a polymorphic variant on purpose, so it cannot be unified with
scalar types *)
type void' = unit

(* Scalar types *)
type int8'	= [`Int8]
type int16'	= [`Int16]
type int32'	= [`Int32]
type int64'	= [`Int64]
type natint'    = [`NatInt]
type uint8'	= [`UInt8]
type uint16'	= [`UInt16]
type uint32'	= [`UInt32]
type uint64'	= [`UInt64]
type bool'	= [`Bool]
type float16'	= [`Float16]
type float32'	= [`Float32]
type float64'	= [`Float64]
(* Builtin type families *)
type sints' = [ `Int8 | `Int16 | `Int32 | `Int64 | `NatInt ]
type uints' = [ `UInt8 | `UInt16 | `UInt32 | `UInt64 ]
type floats' = [ `Float16 | `Float32 | `Float64 ]
type integers' = [sints'|uints']
type numbers' = [integers'|floats']
type signums' = [sints'|floats']
(* Builtin composite types *)
type 'a ptr'
type 'a array'
type 'a struct'
type 'a const

(* C Language description hoisted into OCaml (with some new
constructs), using type witnesses and GADTs for added compile time
safety. (why write a typechecker when you can use OCaml's?) *)

type 'a type'
and ('a,'b) field'
and _ x =
  | XLit	: 'a lit				-> 'a x
  | XOp1	: ('a,'b) op1 * 'a x 			-> 'b x
  | XOp2	: ('a,'b,'c) op2 * 'a x * 'b x		-> 'c x
  | XCall	: ('r,'a) fun' x * ('r,'a) args		-> 'r x
  | XStmtExpr	: st list * 'a x			-> 'a x
  | XIIf	: bool x * 'a x * 'a x			-> 'a x

and st =
  | CLet	: 'a type' * ident * 'a x		-> st
  | CBlock	: st list				-> st
(*| CIf		: int_t x * st * st option		-> st *)
  | CCond	: (bool' x * st) list * st option	-> st
  | CFor	: _ x * bool' x * _ x * st		-> st
  | CSwitch	:
      ([<integers'] as 'a) x * ('a lit * st) list * st option -> st

(* Fully typed argument list/tuple *)
and (_,_) args =
  | AVoid	: ('r,'r) args
  | AApply	: 'a x * ('r,'b) args	-> ('r,'a -> 'b) args

(* Fully typed function signature *)
and (_,_) fun' =
  | FVoid	: 'r type' -> ('r,'r) fun'
  | FLambda	: 'a type' * ident * ('r,'b) fun' -> ('r,'a -> 'b) fun'

and (_,_) op1 =
  | O1Arith	: 'a arith1		-> ('a,'a) op1
  | O1Bit	: [`Not]		-> ([<integers'] as 'a,'a) op1
  | O1Logic	: [`Not]		-> (bool',bool') op1
  | O1Cast	: 'a type'		-> ('a,'b) op1
  | O1Deref	: ('a ptr','a) op1
  | O1Ref	: ('a,'a ptr') op1
  | O1SDeref	: ('a,'b) field'	-> ('a ptr','b) op1
  | O1SRef	: ('a,'b) field'	-> ('a,'b) op1

and (_,_,_) op2 =
  | O2Arith	: 'a arith2			-> ('a,'a,'a) op2
  | O2Comp	: [`Eq|`NE|`Gt|`Lt|`GE|`LE]	-> ([<numbers'|bool'] as 'a,'a,'a) op2
  | O2Logic	: [`And|`Or]			-> (bool',bool',bool') op2
  | O2Bit	: [`And|`Or|`Xor|`Shl|`Shr]	-> ([<integers'] as 'a,'a,'a) op2

and _ arith1 =
  | A1Neg	: [<numbers'] arith1
  | A1PreInc	: [<integers'] arith1
  | A1PreDec	: [<integers'] arith1
  | A1PostInc	: [<integers'] arith1
  | A1PostDec	: [<integers'] arith1

and _ arith2 =
  | A2Add	: [<numbers'] arith2
  | A2Sub	: [<numbers'] arith2
  | A2Mul	: [<numbers'] arith2
  | A2Div	: [<numbers'] arith2
  | A2Mod	: [<integers'] arith2

and _ lit = 
  | LInt8	: int -> int8' lit
  | LInt16	: int -> int16' lit
  | LInt32	: int32 -> int32' lit
  | LInt64	: int64 -> int64' lit
  | LNatInt	: nativeint -> natint' lit
  | LUInt8	: int -> uint8' lit
  | LUInt16	: int -> uint16' lit
  | LUInt32	: int64 -> uint32' lit
  | LUInt64	: int64 -> uint64' lit
  | LFloat32	: float -> float32' lit
  | LFloat64	: float -> float64' lit
  | LQuoted	: string -> 'a lit
  | LStr	: string -> int8' const ptr' lit

and ident = string
and 'a var = 'a type' * ident
and type_repr
and field_repr
and header = [ `Sys of string | `Usr of string ]
