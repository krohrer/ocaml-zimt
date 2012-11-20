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

type lval
type rval

type 'a type'
and ('a,'b) field'
and (_,_) x =
  | XLit : 'a lit						-> ('a,rval) x
  | XLet : 'a type' * ('a,_) x * (('a,_) x -> ('b,_) x)		-> ('a,rval) x
  | XSet : ('a,lval) x * ('a,_) x				-> (void',rval) x
  | XOp1 : ('a,'b,'la,'b) op1 * ('a,'la) x			-> ('b,'lb) x
  | XOp2 : ('a,'b,'c,'la,'lb,'lc) op2 * ('a,'la) x * ('b,'lb) x	-> ('c,'lc) x
  | XCall : (('r,'a) fun',_) x * ('r,'a) args			-> ('r,rval) x
  | XCond : 'a cond						-> ('a,rval) x
  | XLoop : ('a,'b) loop					-> ('b,rval) x
  | XSwitch : ('a,'b) switch					-> ('b,rval) x
  | XNop :							   (void',rval) x
  | XIgnore : (_,_) x						-> (void',rval) x
  | XSequence : (void',_) x * ('a,_) x				-> ('a,rval) x

and ('a,'b) loop = {
    l_init	: 'q    . ('a,'q) x;
    l_cond	: 'q 'r . (('a,'q) x -> (bool','r) x);
    l_step	: 'q 'r . (('a,'q) x -> ('a,'r) x);
    l_body	: 'q 'r . (('a,'q) x -> ('b,'r) x);
  }

and ('a,'b) switch = {
    s_expr	: 'q . ('a,'q) x;
    s_branches	: 'q . ('a lit * ('b,'q) x) list;
    s_else	: 'q . ('a,'q) x;
  }

and 'a cond = {
    c_branches	: 'q 'r . ((bool','q) x * ('a,'r) x) list;
    c_else	: 'q    . ('a,'q) x;
  }

(* Fully typed argument list/tuple *)
and (_,_) args =
  | AVoid	: 				   ('r,'r) args
  | AApply	: ('a,_) x * ('r,'b) args	-> ('r,'a -> 'b) args

(* Fully typed function signature *)
and (_,_) fun' =
  | FVoid	: 'r type'			-> ('r,'r) fun'
  | FLambda	: 'a type' * ('r,'b) fun'	-> ('r,'a -> 'b) fun'

and (_,_, _,_) op1 =
  | O1Arith	: 'a arith1		-> ('a,'a, _,_) op1
  | O1Bit	: [`Not]		-> ([<integers'] as 'a,'a, _,_) op1
  | O1Logic	: [`Not]		-> (bool',bool', _,_) op1
  | O1Cast	: 'a type'		-> ('a,'b, _,rval) op1
  | O1Deref	: 			   ('a ptr','a, _,lval) op1
  | O1SDeref	: ('a,'b) field'	-> ('a ptr','b, _,lval) op1
  | O1Ref	:			   ('a,'a ptr', _,rval) op1
  | O1SRef	: ('a,'b) field'	-> ('a     ,'b, _,lval) op1

and (_,_,_,_,_,_) op2 =
  | O2Arith	: 'a arith2			-> ('a,'a,'a, _,_,rval) op2
  | O2Comp	: [`Eq|`NE|`Gt|`Lt|`GE|`LE]	-> ([<numbers'|bool'] as 'a,'a,'a, _,_,rval) op2
  | O2Logic	: [`And|`Or]			-> (bool',bool',bool', _,_,rval) op2
  | O2Bit	: [`And|`Or|`Xor|`Shl|`Shr]	-> ([<integers'] as 'a,'a,'a, _,_,rval) op2

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
  | LStr	: string -> int8' const ptr' lit

and ident = string
