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

type mut
type imm

type 'a t
and 'a var
and ('r,'a) fun'
and ('a,'b) field'
and (_,_) x =
  | XLit : 'a lit						-> ('a,imm) x
  | XVar : 'a var						-> ('a,_) x
  | XLet : 'a t * ('a,imm) x * ('a var -> ('b,imm) x)		-> ('b,imm) x
  | XFLet : ('r,'a) fsig * ident * 'a				-> (('r,'a) fun',imm) x
  | XSet : ('a,mut) x * ('a,imm) x				-> (void',imm) x
  | XOp1 : ('a,'b, 'r) op1 * ('a,imm) x				-> ('b,'r) x
  | XOp2 : ('a,'b,'c, 'r) op2 * ('a,imm) x * ('b,imm) x		-> ('c,'r) x
  | XCond : 'a cond						-> ('a,imm) x
  | XLoop : ('a,'b) loop					-> ('b,imm) x
  | XSwitch : ('a,'b) switch					-> ('b,imm) x
  | XNop :							   (void',imm) x
  | XIgnore : (_,imm) x						-> (void',imm) x
  | XSeq : (void',imm) x * ('a,imm) x				-> ('a,imm) x
  | XCall : (('r,'a) fun',imm) x * ('r,'a) args			-> ('r,imm) x

and ('a,'b) loop = {
    l_init	: ('a,imm) x;
    l_cond	: (('a,imm) x -> (bool',imm) x);
    l_step	: (('a,imm) x -> ('a,imm) x);
    l_body	: (('a,imm) x -> ('b,imm) x);
  }

and ('a,'b) switch = {
    s_expr	: ('a,imm) x;
    s_branches	: ('a lit * ('b,imm) x) list;
    s_else	: ('a,imm) x;
  }

and 'a cond = {
    c_branches	: ((bool',imm) x * ('a,imm) x) list;
    c_else	: ('a,imm) x;
  }

(* Fully typed argument list/tuple *)
and (_,_) args =
  | AVoid	: 				   ('r,('r,imm) x) args
  | AApply	: ('a,imm) x * ('r,'b) args	-> ('r,'a var -> 'b) args

(* Fully typed function signature *)
and (_,_) fsig =
  | FVoid	: 'r t			-> ('r,('r,imm) x) fsig
  | FLambda	: 'a t * ('r,'b) fsig	-> ('r,'a var -> 'b) fsig

and (_,_, _) op1 =
  | O1Arith	: 'a arith1		-> ('a,'a, imm) op1
  | O1Bit	: [`Not]		-> ([<integers'] as 'a,'a, imm) op1
  | O1Logic	: [`Not]		-> (bool',bool', imm) op1
  | O1Cast	: 'a t			-> ('a,'b, imm) op1
  | O1Deref	: 			   ('a ptr','a, _) op1
  | O1SDeref	: ('a,'b) field'	-> ('a ptr','b, _) op1
  | O1Ref	:			   ('a,'a ptr', _) op1
  | O1SRef	: ('a,'b) field'	-> ('a     ,'b, _) op1

and (_,_,_, _) op2 =
  | O2Arith	: 'a arith2			-> ('a,'a,'a, imm) op2
  | O2PArith	: [`Add|`Sub]			-> ('a ptr',[<integers'],'a ptr', imm) op2
  | O2Comp	: [`Eq|`NE|`Gt|`Lt|`GE|`LE]	-> ([<numbers'|bool'] as 'a,'a,'a, imm) op2
  | O2Logic	: [`And|`Or]			-> (bool',bool',bool', imm) op2
  | O2Bit	: [`And|`Or|`Xor|`Shl|`Shr]	-> ([<integers'] as 'a,'a,'a, imm) op2

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
