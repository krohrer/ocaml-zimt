type rank = int
type associativity = [`L2R|`R2L]

include C_UntypedAST

module Type =
  struct
    let f'id _ a = a

    let fold_right
      ~(f'void	= f'id)
      ~(f'bool	= f'id)
      ~(f'int	= f'id)
      ~(f'real	= f'id)
      ~(f'ref	= f'id)
      ~(f'ptr	= f'id)
      ~(f'func	= f'id)
      ~(f'arr	= f'id) =
      fun (quals, spec) ->
	match spec with
	| TVoid		-> f'void (quals  )
	| TBool		-> f'bool (quals  )
	| TInt s	-> f'int  (quals,s)
	| TReal s	-> f'real (quals,s)
	| TRef s	-> f'ref  (quals,s)
	| TPtr s	-> f'ptr  (quals,s)
	| TFunc s	-> f'func (quals,s)
	| TArr s	-> f'arr  (quals,s)

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
      | XIdent _
      | XStmtExpr _
      | XInit _
      | XDInit _			-> 0
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
      | Op2Assign			-> 15
      | Op2Comma			-> comma_precedence
    and comma_precedence		=  17
  end

module Stmt =
  struct
    type t = C_UntypedAST.st
  end

module Embedded =
  struct
    include C_UntypedAST

    let var n		= XIdent n

    let call n args	= XCall (XIdent n, args)
    let apply x args	= XCall (x, args)

    let ( ~- ) x 	= XOp1 (Op1Arith `Neg, x)
    let inc x		= XOp1 (Op1Arith `PreInc, x)
    let dec x		= XOp1 (Op1Arith `PreDec, x)
    let postinc x	= XOp1 (Op1Arith `PostInc, x)
    let postdec x	= XOp1 (Op1Arith `PostDec, x)
    let not x		= XOp1 (Op1Logic `Not, x)
    let lnot x		= XOp1 (Op1Bit `Not, x)
    let cast t x	= XOp1 (Op1Cast t, x)
    let ( ^! ) x f	= XOp1 (Op1StructDeref f, x)
    let ( ^ ) x f	= XOp1 (Op1StructRef f, x)
    let ( ! ) x		= XOp1 (Op1Deref, x)
    let ref x		= XOp1 (Op1Ref, x)

    let ( := ) x y	= XOp2 (Op2Assign, x, y)
    let ( +! ) x y	= XOp2 (Op2Subscript, x, y)

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
  end
