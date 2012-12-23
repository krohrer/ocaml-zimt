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
  | O1Arith `PostInc
  | O1Arith `PostDec
  | O1StructDeref _
  | O1StructRef _		-> 2
  | O1Arith `Neg
  | O1Arith `PreInc
  | O1Arith `PreDec
  | O1Bit `Not
  | O1Logic `Not
  | O1Cast _
  | O1Deref
  | O1Ref			-> 3
and op2_precedence = function
  | O2Subscript			-> 2
  | O2Arith `Mul
  | O2Arith `Div
  | O2Arith `Mod		-> 5
  | O2Arith `Add
  | O2Arith `Sub		-> 6
  | O2Bit `ShiftL
  | O2Bit `ShiftR		-> 7
  | O2Comp `Gt
  | O2Comp `Lt
  | O2Comp `GE
  | O2Comp `LE			-> 8
  | O2Comp `Eq
  | O2Comp `NE			-> 9
  | O2Bit `And			-> 10
  | O2Bit `Xor			-> 11
  | O2Bit `Or			-> 12
  | O2Logic `And		-> 13
  | O2Logic `Or			-> 14
  | O2Assign			-> assign_precedence
  | O2Comma			-> comma_precedence
and assign_precedence		=  15
and comma_precedence		=  17

let simplify x = x
