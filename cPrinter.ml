open C

type formatter = Format.formatter

let print_expression fmt =
  failwith ""

let print_statement fmt =
  failwith ""

let print_translation_unit fmt =
  failwith ""

(* http://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence *)

type precedence = [ `L2R of int | `R2L of int | `Undef ]

let rec expression_precedence = function
  | XQuote _
  | XLit _
  | XVar _
  | XStmtExpr _
  | XInit _
  | XDInit _		-> `Undef

  | XCall _		-> `L2R 2

  | XOp1 (o,_)		-> op1_precedence o
  | XOp2 (o,_,_)	-> op2_precedence o
 
  | XIIf (_,_,_)	-> `R2L 15

and op1_precedence = function
  | Op1Arith `PostInc
  | Op1Arith `PostDec
  | Op1SDeref _
  | Op1SRef _		-> `L2R 2

  | Op1Arith `Neg
  | Op1Arith `PreInc
  | Op1Arith `PreDec
  | Op1Bit `Not
  | Op1Logic `Not
  | Op1Cast _
  | Op1Deref
  | Op1Ref		-> `R2L 3

and op2_precedence = function
  | Op2ArrSubs 		-> `L2R 2

  | Op2Arith `Mul
  | Op2Arith `Div
  | Op2Arith `Mod	-> `L2R 5

  | Op2Arith `Add
  | Op2Arith `Sub	-> `L2R 6

  | Op2Bit `Shl
  | Op2Bit `Shr		-> `L2R 7

  | Op2Comp `Gt
  | Op2Comp `Lt
  | Op2Comp `GE
  | Op2Comp `LE		-> `L2R 8

  | Op2Comp `Eq
  | Op2Comp `NE		-> `L2R 9

  | Op2Bit `And		-> `L2R 10

  | Op2Bit `Xor		-> `L2R 11

  | Op2Bit `Or		-> `L2R 12

  | Op2Logic `And	-> `L2R 13

  | Op2Logic `Or	-> `L2R 14

  | Op2Assign	 	-> `R2L 15

  | Op2Comma		-> `L2R 17


