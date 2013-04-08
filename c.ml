(* TODO : Rethink type qualifiers, I guess you are bound to take some
   liberties with the definitions when not following the C99 spec to
   the word. (And I'm not going to read the whole thing just now.)
*)
type t			=
  | TVoid
  | TBool		of type_qual list
  | TInt		of type_qual list * sign_spec * int_t
  | TReal		of type_qual list * real_t
  | TPtr		of type_qual list * t
  | TNamed		of type_qual list * named_t

  | TFunc		of func_t
  | TArr		of arr_t

  | TStruct		of ident option * struct_t
  | TUnion		of ident option * union_t
  | TEnum		of ident option * enum_t

and prim_t		=
  | PBool
  | PInt		of sign_spec * int_t
  | PReal		of real_t

and type_qual		=
  | Const
  | Restrict
  | Volatile

and ident		= string

and sign_spec		=
  | Signed
  | Unsigned
  | DefaultSign

and int_t		= 
  | Char
  | Short
  | Int
  | Long
  | LongLong

and real_t		=
  | Float
  | Double
  | LongDouble

and named_t		=
  | NamedStruct		of ident
  | NamedUnion		of ident
  | NamedEnum		of ident
  | Typedef		of ident

and func_t		= t * func_arg list * arity
and func_arg		= t * ident option
and arity		=
  | Variadic
  | Fixed

and arr_t		= t * array_sizes
and array_sizes		= int list

and struct_t		= field_decl list
and union_t		= field_decl list
and field_decl		= 
  | Field		of t * ident
  | BitsField		of t * ident * int
  | BitsPadding		of t * int

and enum_t		= enum_decl list
and enum_decl		= ident * x option


and declaration		= storage_class option * t * ident
and storage_class	=
  | Extern
  | Static
  | Auto
  | Register


and defval		=
  | Defvar		of defvar
  | Defunc		of defunc

and typedef		= t * ident
and defvar		= declaration * x
and defunc		= declaration * code


and field		= ident

and x			=
  | XQuote		of string
  | XLit		of lit
  | XId			of ident
  | XCall		of x * x list
  | XOp1		of op1 * x
  | XOp2		of op2 * x * x
  | XStmtExpr		of code list
  | XIIf		of x * x * x
  | XInit		of init list

and init		= x

and code		=
  | CEmpty
  | CExpr		of x
  | CSeq		of code list
  | CBlock		of code list
  | CDef		of defvar
  | CSwitch		of x * code
  | CLabeled		of label * code
  | CGoto		of ident
  | CFor		of s_for * code
  | CWhile		of x * code
  | CDoWhile		of code * x
  | CIf			of x * code * code
  | CBreak	
  | CContinue
  | CReturn		of x
  | CReturn0

and s_for		= [`none | `def of defvar | `expr of x] * x option * x option

and label		=
  | CaseConst		of lit
  | CaseNamed		of ident
  | CaseDefault
  | Label		of ident

and op1			=
  | O1Arith		of [`Neg|`PreInc|`PreDec|`PostInc|`PostDec]
  | O1Bit		of [`Not]
  | O1Logic		of [`Not]
  | O1Cast		of t
  | O1Deref
  | O1StructDeref	of field
  | O1Ref
  | O1StructRef	of field

and op2			=
  | O2Assign
  | O2Subscript
  | O2Arith		of [`Add|`Sub|`Mul|`Div|`Mod]
  | O2Comp		of [`Eq|`NE|`Gt|`Lt|`GE|`LE]
  | O2Logic		of [`And|`Or]
  | O2Bit		of [`And|`Or|`Xor|`ShiftL|`ShiftR]
  | O2Comma

and lit			= 
  | LQuote		of string
  | LInt		of int
  | LInt32		of int32
  | LInt64		of int64
  | LUInt		of int
  | LUInt32		of int32
  | LUInt64		of int64
  | LFloat32		of float
  | LFloat64		of float
  | LStr		of string
