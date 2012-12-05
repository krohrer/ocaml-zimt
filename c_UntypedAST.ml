type t =
  | TVoid
  | TPrim		of type_qual list * prim_t
  | TRef		of type_qual list * ref_t
  | TPtr		of type_qual list * t
  | TFunc		of func_t
  | TArr		of arr_t
and prim_t =
  | PBool
  | PInt		of int_t
  | PReal		of real_t

and type_qual		= [`const | `restrict | `volatile]
and ident		= string

and int_t		= sign_spec * [`char | `short | `int | `long | `longlong]
and sign_spec		= [`unsigned | `signed | `default]

and real_t		= [`float | `double | `longdouble ]

and ref_t		= [`struct' | `union | `enum | `typedef] * ident

and func_t		= t * arg list * arity
and arg			= t
and arity		= [`variadic | `fixed]

and arr_t		= t * array_sizes
and array_sizes		= int list

and field_decl = 
  | FField		of t * ident
  | FBitField		of t * ident * int
  | FBitPadding		of int_t * int

and enumerator		= ident * x option
and declaration		= storage_class * t * ident
and storage_class	= [`extern | `static | `auto | `register]
and field		= ident

and x =
  | XQuote		of string
  | XLit		of lit
  | XIdent		of ident
  | XCall		of x * x list
  | XOp1		of op1 * x
  | XOp2		of op2 * x * x
  | XStmtExpr		of st list
  | XIIf		of x * x * x
  | XInit		of x list
  | XDInit		of (ident * x) list

and st =
  | StExpr		of x
  | StBlock		of st list
  | StDecl		of t * ident * x option
  | StSwitch		of x * (lit * st list) * st list
  | StFor		of x * x * x * st
  | StWhile		of x * st
  | StDoWhile		of st * x
  | StIf		of x * x * x
  | StNop
  | StBreak
  | StContinue
  | StReturn		of x

and decl =
  | DFun		of t * ident * t list * x
  | DVar		of t * ident * x option

and tlunit = decl list

and op1 =
  | Op1Arith		of [`Neg|`PreInc|`PreDec|`PostInc|`PostDec]
  | Op1Bit		of [`Not]
  | Op1Logic		of [`Not]
  | Op1Cast		of t
  | Op1Deref
  | Op1StructDeref	of field
  | Op1Ref
  | Op1StructRef	of field

and op2 =
  | Op2Assign
  | Op2Subscript
  | Op2Arith		of [`Add|`Sub|`Mul|`Div|`Mod]
  | Op2Comp		of [`Eq|`NE|`Gt|`Lt|`GE|`LE]
  | Op2Logic		of [`And|`Or]
  | Op2Bit		of [`And|`Or|`Xor|`ShiftL|`ShiftR]
  | Op2Comma

and lit = 
  | LInt		of int
  | LInt32		of int32
  | LInt64		of int64
  | LUInt		of int
  | LUInt32		of int32
  | LUInt64		of int64
  | LFloat32		of float
  | LFloat64		of float
  | LStr		of string