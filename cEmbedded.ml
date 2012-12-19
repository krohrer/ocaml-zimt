type x = C.x
type t = C.t
type s = C.s
type lit = C.lit
type ident = C.ident
type field = C.field

open C

    (* Expressions *)
let var n		= XId n

let call n args		= XCall (XId n, args)
let apply x args	= XCall (x, args)

let ( ~- ) x		= XOp1 (Op1Arith `Neg, x)
let inc x		= XOp1 (Op1Arith `PreInc, x)
let dec x		= XOp1 (Op1Arith `PreDec, x)
let postinc x		= XOp1 (Op1Arith `PostInc, x)
let postdec x		= XOp1 (Op1Arith `PostDec, x)
let not x		= XOp1 (Op1Logic `Not, x)
let lnot x		= XOp1 (Op1Bit `Not, x)
let cast t x		= XOp1 (Op1Cast t, x)
let ( **! ) x f		= XOp1 (Op1StructDeref f, x)
let sderef x f		= x**!f
let ( **. ) x f		= XOp1 (Op1StructRef f, x)
let sref x f		= x**.f
let ( ! ) x		= XOp1 (Op1Deref, x)
let ref x		= XOp1 (Op1Ref, x)

let ( := ) x y		= XOp2 (Op2Assign, x, y)
let ( **@ ) x y		= XOp2 (Op2Subscript, x, y)
let idx x y		= x **@ y

let ( + ) x y		= XOp2 (Op2Arith `Add, x, y)
let ( - ) x y		= XOp2 (Op2Arith `Sub, x, y)
let ( * ) x y		= XOp2 (Op2Arith `Mul, x, y)
let ( / ) x y		= XOp2 (Op2Arith `Div, x, y)
let ( mod ) x y		= XOp2 (Op2Arith `Mod, x, y)

let ( = ) x y 		= XOp2 (Op2Comp `Eq, x, y)
let ( == ) x y	 	= XOp2 (Op2Comp `Eq, x, y)
let ( <> ) x y		= XOp2 (Op2Comp `NE, x, y)
let ( != ) x y		= XOp2 (Op2Comp `NE, x, y)
let ( > ) x y		= XOp2 (Op2Comp `Gt, x, y)
let ( < ) x y		= XOp2 (Op2Comp `Lt, x, y)
let ( >= ) x y		= XOp2 (Op2Comp `GE, x, y)
let ( <= ) x y	 	= XOp2 (Op2Comp `LE, x, y)

let ( && ) x y		= XOp2 (Op2Logic `And, x, y)
let ( || ) x y		= XOp2 (Op2Logic `Or, x, y)

let ( land ) x y	= XOp2 (Op2Bit `And, x, y)
let ( lor ) x y		= XOp2 (Op2Bit `Or, x, y)
let ( lxor ) x y	= XOp2 (Op2Bit `Xor, x, y)
let ( lsl ) x y		= XOp2 (Op2Bit `ShiftL, x, y)
let ( lsr ) x y		= XOp2 (Op2Bit `ShiftR, x, y)

    (* Literals *)
let intlit i		= XLit (LInt i)
let strlit s		= XLit (LStr s)

    (* Statements *)
let block sl		= SBlock sl

let expr x		= SExpr x
let decl t n x		= SDecl (t, n, Some x)

let switch x sl		= SSwitch (x, block sl)

let switch x sl		= SSwitch (x, SBlock sl)
let case n sl		= SBlock (SLabeled (CaseNamed n, SEmpty) :: sl)
let lcase l	sl	= SBlock (SLabeled (CaseConst l, SEmpty) :: sl)
let default sl		= SBlock (SLabeled (CaseDefault, SEmpty) :: sl)

let for_ever sl		= SFor ((`none, None, None), SBlock sl)
let for' t n x c i sl	= SFor ((`decl (t, n, Some x),
				 Some c,
				 Some i),
				SBlock sl)

let while' x sl		= SWhile (x, SBlock sl)
let do_while sl x	= SDoWhile (SBlock sl, x)
let if' c tl el		= SIf (c, SBlock tl, SBlock el)

let return x		= SReturn x
let break 		= SBreak
let continue		= SContinue
let goto n		= SGoto n
let label n		= SLabeled (Label n, SEmpty)

