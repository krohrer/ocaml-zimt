type x = C.x
type t = C.t
type code = C.code
type lit = C.lit
type ident = C.ident
type field = C.field

open C

    (* Expressions *)
let var n		= XId n

let call n args		= XCall (XId n, args)
let apply x args	= XCall (x, args)

let ( ~- ) x		= XOp1 (O1Arith `Neg, x)
let inc x		= XOp1 (O1Arith `PreInc, x)
let dec x		= XOp1 (O1Arith `PreDec, x)
let postinc x		= XOp1 (O1Arith `PostInc, x)
let postdec x		= XOp1 (O1Arith `PostDec, x)
let not x		= XOp1 (O1Logic `Not, x)
let lnot x		= XOp1 (O1Bit `Not, x)
let cast t x		= XOp1 (O1Cast t, x)
let ( **! ) x f		= XOp1 (O1StructDeref f, x)
let sderef x f		= x**!f
let ( **. ) x f		= XOp1 (O1StructRef f, x)
let sref x f		= x**.f
let ( ! ) x		= XOp1 (O1Deref, x)
let ref x		= XOp1 (O1Ref, x)

let ( := ) x y		= XOp2 (O2Assign, x, y)
let ( **@ ) x y		= XOp2 (O2Subscript, x, y)
let idx x y		= x **@ y

let ( + ) x y		= XOp2 (O2Arith `Add, x, y)
let ( - ) x y		= XOp2 (O2Arith `Sub, x, y)
let ( * ) x y		= XOp2 (O2Arith `Mul, x, y)
let ( / ) x y		= XOp2 (O2Arith `Div, x, y)
let ( mod ) x y		= XOp2 (O2Arith `Mod, x, y)

let ( = ) x y 		= XOp2 (O2Comp `Eq, x, y)
let ( == ) x y	 	= XOp2 (O2Comp `Eq, x, y)
let ( <> ) x y		= XOp2 (O2Comp `NE, x, y)
let ( != ) x y		= XOp2 (O2Comp `NE, x, y)
let ( > ) x y		= XOp2 (O2Comp `Gt, x, y)
let ( < ) x y		= XOp2 (O2Comp `Lt, x, y)
let ( >= ) x y		= XOp2 (O2Comp `GE, x, y)
let ( <= ) x y	 	= XOp2 (O2Comp `LE, x, y)

let ( && ) x y		= XOp2 (O2Logic `And, x, y)
let ( || ) x y		= XOp2 (O2Logic `Or, x, y)

let ( land ) x y	= XOp2 (O2Bit `And, x, y)
let ( lor ) x y		= XOp2 (O2Bit `Or, x, y)
let ( lxor ) x y	= XOp2 (O2Bit `Xor, x, y)
let ( lsl ) x y		= XOp2 (O2Bit `ShiftL, x, y)
let ( lsr ) x y		= XOp2 (O2Bit `ShiftR, x, y)

    (* Literals *)
let intlit i		= XLit (LInt i)
let strlit s		= XLit (LStr s)

    (* Statements *)
let block sl		= CBlock sl

let expr x		= CExpr x
let def t n x		= CDef ((None,t,n), x)

let switch x sl		= CSwitch (x, block sl)

let switch x sl		= CSwitch (x, CBlock sl)
let case n sl		= CBlock (CLabeled (CaseNamed n, CEmpty) :: sl)
let lcase l	sl	= CBlock (CLabeled (CaseConst l, CEmpty) :: sl)
let default sl		= CBlock (CLabeled (CaseDefault, CEmpty) :: sl)

let for_ever sl		= CFor ((`none, None, None), CBlock sl)
let for' t n x c i sl	= CFor ((`def ((None,t,n), x),
				 Some c,
				 Some i),
				CBlock sl)

let while' x sl		= CWhile (x, CBlock sl)
let do_while sl x	= CDoWhile (CBlock sl, x)
let if' c tl el		= CIf (c, CBlock tl, CBlock el)

let return x		= CReturn x
let break 		= CBreak
let continue		= CContinue
let goto n		= CGoto n
let label n		= CLabeled (Label n, CEmpty)

