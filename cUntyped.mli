type t = string
and field' = string
and x =
  | XQuote of string
  | XLit of lit
  | XVar of ident
  | XCall of x * x array
  | XOp1 of op1 * x
  | XOp2 of op2 * x * x
  | XStmtExpr of st list * x
  | XIIf of x * x * x

and st =
  | StExpr of x
  | StBlock of st list
  | StDecl of t * ident * x option
  | StSwitch of x * (lit * st list) * st list
  | StFor of x * x * x * st
  | StWhile of x * st
  | StDoWhile of st * x
  | StIf of x * x * x
  | StNop
  | StBreak
  | StContinue

and decl =
  | DFun of t * ident * t array * x
  | DVar of t * ident * x option

and tlunit = decl list

and switch = {
    s_expr	: x;
    s_branches	: (lit * x) list;
    s_else	: x;
  }

and op1 =
  | Op1Arith of [`Neg|`PreInc|`PreDec|`PostInc|`PostDec]
  | Op1Bit of [`Not]
  | Op1Logic of [`Not]
  | Op1Cast of t
  | Op1Deref
  | Op1SDeref of field'
  | Op1Ref
  | Op1SRef of field'

and op2 =
  | Op2Assign
  | Op2Arith of [`Add|`Sub|`Mul|`Div|`Mod]
  | Op2PArith of [`Add|`Sub]
  | Op2Comp of [`Eq|`NE|`Gt|`Lt|`GE|`LE]
  | Op2Logic of [`And|`Or]
  | Op2Bit of [`And|`Or|`Xor|`Shl|`Shr]

and lit = 
  | LInt8 of int
  | LInt16 of int
  | LInt32 of int32
  | LInt64 of int64
  | LNatInt of nativeint
  | LUInt8 of int
  | LUInt16 of int
  | LUInt32 of int32
  | LUInt64 of int64
  | LFloat32 of float
  | LFloat64 of float
  | LStr of string

and ident = string
