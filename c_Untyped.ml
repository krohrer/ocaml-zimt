type rank = int
type associativity = [`L2R|`R2L]

include C_UntypedAST

module Type =
  struct
  end

module Expr =
  struct
    type t = C_UntypedAST.x

    open C_UntypedAST
    
    (* http://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence *)

    (* Highest precedence is rank 0 for atomic expressions. Higher
       ranks mean lower precedence. *)

    let associativity = function
      | x when x < 3	-> `L2R
      | x when x < 4	-> `R2L
      | x when x < 15	-> `L2R
      | x when x < 17	-> `R2L
      | _		-> `L2R

    let rec precedence = function
      (* These are either atomic, or properly bracketed. *)
      | XId _
      | XStmtExpr _
      | XInit _				-> 0
      | XCall _				-> call_precedence
      | XLit l				-> lit_precedence l
      | XOp1 (o,_)			-> op1_precedence o
      | XOp2 (o,_,_)			-> op2_precedence o
      | XIIf (_,_,_)			-> 15
      (* We don't know what the quoted string contains, so we simply
	 give it a very low precedence *)
      | XQuote s			-> 999999
    and call_precedence			=  2
    and lit_precedence = function
      (* Negative numbers have the same precedence as Op1Arith `Neg *)
      | LInt i	when i < 0		-> 3
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
      | Op1StructRef _			-> 2
      | Op1Arith `Neg
      | Op1Arith `PreInc
      | Op1Arith `PreDec
      | Op1Bit `Not
      | Op1Logic `Not
      | Op1Cast _
      | Op1Deref
      | Op1Ref				-> 3
    and op2_precedence = function
      | Op2Subscript			-> 2
      | Op2Arith `Mul
      | Op2Arith `Div
      | Op2Arith `Mod			-> 5
      | Op2Arith `Add
      | Op2Arith `Sub			-> 6
      | Op2Bit `ShiftL
      | Op2Bit `ShiftR			-> 7
      | Op2Comp `Gt
      | Op2Comp `Lt
      | Op2Comp `GE
      | Op2Comp `LE			-> 8
      | Op2Comp `Eq
      | Op2Comp `NE			-> 9
      | Op2Bit `And			-> 10
      | Op2Bit `Xor			-> 11
      | Op2Bit `Or			-> 12
      | Op2Logic `And			-> 13
      | Op2Logic `Or			-> 14
      | Op2Assign			-> assign_precedence
      | Op2Comma			-> comma_precedence
    and assign_precedence		=  15
    and comma_precedence		=  17
  end

module Stmt =
  struct
  end

module Embedded =
  struct
    open C_UntypedAST

    (* Expressions *)
    let var n		= XId n

    let call n args	= XCall (XId n, args)
    let apply x args	= XCall (x, args)

    let ( ~- ) x 	= XOp1 (Op1Arith `Neg, x)
    let inc x		= XOp1 (Op1Arith `PreInc, x)
    let dec x		= XOp1 (Op1Arith `PreDec, x)
    let postinc x	= XOp1 (Op1Arith `PostInc, x)
    let postdec x	= XOp1 (Op1Arith `PostDec, x)
    let not x		= XOp1 (Op1Logic `Not, x)
    let lnot x		= XOp1 (Op1Bit `Not, x)
    let cast t x	= XOp1 (Op1Cast t, x)
    let ( **! ) x f	= XOp1 (Op1StructDeref f, x)
    let sderef x f	= x**!f
    let ( **. ) x f	= XOp1 (Op1StructRef f, x)
    let sref x f	= x**.f
    let ( ! ) x		= XOp1 (Op1Deref, x)
    let ref x		= XOp1 (Op1Ref, x)

    let ( := ) x y	= XOp2 (Op2Assign, x, y)
    let ( **@ ) x y	= XOp2 (Op2Subscript, x, y)
    let idx x y		= x **@ y

    let ( + ) x y	= XOp2 (Op2Arith `Add, x, y)
    let ( - ) x y	= XOp2 (Op2Arith `Sub, x, y)
    let ( * ) x y	= XOp2 (Op2Arith `Mul, x, y)
    let ( / ) x y	= XOp2 (Op2Arith `Div, x, y)
    let ( mod ) x y	= XOp2 (Op2Arith `Mod, x, y)

    let ( = ) x y 	= XOp2 (Op2Comp `Eq, x, y)
    let ( == ) x y 	= XOp2 (Op2Comp `Eq, x, y)
    let ( <> ) x y	= XOp2 (Op2Comp `NE, x, y)
    let ( != ) x y	= XOp2 (Op2Comp `NE, x, y)
    let ( > ) x y	= XOp2 (Op2Comp `Gt, x, y)
    let ( < ) x y	= XOp2 (Op2Comp `Lt, x, y)
    let ( >= ) x y 	= XOp2 (Op2Comp `GE, x, y)
    let ( <= ) x y 	= XOp2 (Op2Comp `LE, x, y)

    let ( && ) x y	= XOp2 (Op2Logic `And, x, y)
    let ( || ) x y	= XOp2 (Op2Logic `Or, x, y)

    let ( land ) x y	= XOp2 (Op2Bit `And, x, y)
    let ( lor ) x y	= XOp2 (Op2Bit `Or, x, y)
    let ( lxor ) x y	= XOp2 (Op2Bit `Xor, x, y)
    let ( lsl ) x y	= XOp2 (Op2Bit `ShiftL, x, y)
    let ( lsr ) x y	= XOp2 (Op2Bit `ShiftR, x, y)

    (* Literals *)
    let intlit i	= XLit (LInt i)
    let strlit s	= XLit (LStr s)

    (* Statements *)
    let block sl	= SBlock sl

    let expr x		= SExpr x
    let decl t n x	= SDecl (t, n, Some x)

    let switch x sl	= SSwitch (x, block sl)

    let switch x sl	= SSwitch (x, SBlock sl)
    let case n sl	= SBlock (SLabeled (CaseNamed n, SEmpty) :: sl)
    let lcase l	sl	= SBlock (SLabeled (CaseConst l, SEmpty) :: sl)
    let default sl	= SBlock (SLabeled (CaseDefault, SEmpty) :: sl)

    let for_ever sl		= SFor ((`none, None, None), SBlock sl)
    let for' t n x c i sl	= SFor ((`decl (t, n, Some x),
					 Some c,
					 Some i),
					SBlock sl)

    let while' x sl	= SWhile (x, SBlock sl)
    let do_while sl x	= SDoWhile (SBlock sl, x)
    let if' c tl el	= SIf (c, SBlock tl, SBlock el)

    let return x	= SReturn x
    let break 		= SBreak
    let continue 	= SContinue
    let goto n		= SGoto n
    let label n		= SLabeled (Label n, SEmpty)
  end
