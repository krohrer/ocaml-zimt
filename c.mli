type t = {
    t_qualifiers : qualifiers;
    t_requires : header list;
    t_metatype : metatype;
  }

and metatype =
  | TFPtr of t * t array
  | TPtr of t
  | TArray of t * int
  | TEnum of ident option * (lit * x option) list
  | TUnion of ident option * field list
  | TStruct of ident option * field list
  | TBool
  | TInt of bits
  | TUnsigned of bits
  | TFloat
  | TDouble
  | TCamlValue
  | TVoid
  | TRef of ident
  | TRefEnum of ident
  | TRefUnion of ident
  | TRefStruct of ident

and bits = int
and qualifiers = [`Const|`Static|`Extern] list

and ident = string
and header = string
and field = string

and x =
  | XQuote of string
  | XLit of lit
  | XIdent of ident
  | XCall of x * x list
  | XOp1 of op1 * x
  | XOp2 of op2 * x * x
  | XStmtExpr of st list
  | XIIf of x * x * x
  | XInit of x list
  | XDInit of (ident * x) list

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
  | StReturn of x

and decl =
  | DFun of t * ident * t list * x
  | DVar of t * ident * x option

and tlunit = decl list

and op1 =
  | Op1Arith of [`Neg|`PreInc|`PreDec|`PostInc|`PostDec]
  | Op1Bit of [`Not]
  | Op1Logic of [`Not]
  | Op1Cast of t
  | Op1Deref
  | Op1SDeref of field
  | Op1Ref
  | Op1SRef of field

and op2 =
  | Op2Assign
  | Op2ArrSubs
  | Op2Arith of [`Add|`Sub|`Mul|`Div|`Mod]
  | Op2Comp of [`Eq|`NE|`Gt|`Lt|`GE|`LE]
  | Op2Logic of [`And|`Or]
  | Op2Bit of [`And|`Or|`Xor|`Shl|`Shr]
  | Op2Comma

and lit = 
  | LInt of int
  | LInt32 of int32
  | LInt64 of int64

  | LUInt of int
  | LUInt32 of int32
  | LUInt64 of int64

  | LFloat32 of float
  | LFloat64 of float

  | LStr of string
