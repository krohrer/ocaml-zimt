type rank = int
type associativity = [`L2R|`R2L]

open C
    
(* http://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence *)

(* Highest precedence is rank 0 for atomic expressions. Higher
   ranks mean lower precedence. *)

let associativity = function
  | x when x < 3	-> `L2R
  | x when x < 4	-> `R2L
  | x when x < 15	-> `L2R
  | x when x < 17	-> `R2L
  | _			-> `L2R

let rec precedence = function
  (* These are either atomic, or properly bracketed. *)
  | XId _
  | XStmtExpr _
  | XInit _			-> 0
  | XCall _			-> call_precedence
  | XLit l			-> lit_precedence l
  | XOp1 (o,_)			-> op1_precedence o
  | XOp2 (o,_,_)		-> op2_precedence o
  | XIIf (_,_,_)		-> 15
  (* We don't know what the quoted string contains, so we simply
     give it a very low precedence *)
  | XQuote s			-> 999999
and call_precedence		=  2
and lit_precedence = function
      (* Negative numbers have the same precedence as Op1Arith `Neg *)
  | LInt i	when i < 0	-> 3
  | LInt32 i	when i < 0l	-> 3
  | LInt64 i	when i < 0L	-> 3
  | LFloat32 f	when f < 0.0	-> 3
  | LFloat64 f	when f < 0.0	-> 3 
      (* The rest is atomic *)
  | _				-> 0
and op1_precedence = function
  | Op1Arith `PostInc
  | Op1Arith `PostDec
  | Op1StructDeref _
  | Op1StructRef _		-> 2
  | Op1Arith `Neg
  | Op1Arith `PreInc
  | Op1Arith `PreDec
  | Op1Bit `Not
  | Op1Logic `Not
  | Op1Cast _
  | Op1Deref
  | Op1Ref			-> 3
and op2_precedence = function
  | Op2Subscript		-> 2
  | Op2Arith `Mul
  | Op2Arith `Div
  | Op2Arith `Mod		-> 5
  | Op2Arith `Add
  | Op2Arith `Sub		-> 6
  | Op2Bit `ShiftL
  | Op2Bit `ShiftR		-> 7
  | Op2Comp `Gt
  | Op2Comp `Lt
  | Op2Comp `GE
  | Op2Comp `LE			-> 8
  | Op2Comp `Eq
  | Op2Comp `NE			-> 9
  | Op2Bit `And			-> 10
  | Op2Bit `Xor			-> 11
  | Op2Bit `Or			-> 12
  | Op2Logic `And		-> 13
  | Op2Logic `Or		-> 14
  | Op2Assign			-> assign_precedence
  | Op2Comma			-> comma_precedence
and assign_precedence		=  15
and comma_precedence		=  17
