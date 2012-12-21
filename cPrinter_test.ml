open C
open PrettyPrinter
open CPrinter

let int		= TInt ([],DefaultSign,Int)
let void	= TVoid
let vptr	= TPtr ([Const], TVoid)
let fptr	= TPtr ([Const], TFunc (void, [void], Fixed))
let fpa		= TArr (fptr, [-1])
let mfp f	= TPtr ([Const], TFunc (void, [void; void], Fixed))

let ptr t = TPtr ([], t)
let func t args = TFunc (t, args, Fixed) 
let arr t sizes = TArr (t, sizes)
let add_const qs = if List.mem Const qs then qs else Const :: qs
let const = function
  | TBool qs		-> TBool (add_const qs)
  | TInt (qs,s,i)	-> TInt (add_const qs,s,i)
  | TReal (qs,r)	-> TReal (add_const qs,r)
  | TPtr (qs,p)		-> TPtr (add_const qs,p)
  | TNamed (qs,n)	-> TNamed (add_const qs,n)
  | x			-> x

let ($) f x = f x
let ( *** ) f x = f x

let _ =
  let decl n t = pp_hbox *** pp_decl t n None +++ pp_cut in
  let pp = 
    pp_vbox ~ind:0 *** pp_seq [
      decl "a" $ void;
      decl "b" $ int;
      decl "c" $ ptr int;
      decl "d" $ const (ptr int);
      decl "e" $ const (ptr (const int));
      decl "f" $ func (const int) [];
      decl "g" $ func void [ptr void; ptr (const int)];
      decl "h" $ arr (arr (ptr (ptr (func void [ptr void; const (ptr (func void [ptr void; ptr (const int)]))]))) [1]) [2];
      decl "i" $ arr (arr int [1;2]) [3];
      decl "j" $ func void [int; int; ptr void; arr (const int) [5;4]];
      pp_code (CBlock [
	CDecl (fptr, "f", None);
	CBlock [
	  CExpr (XIIf (XQuote "X",
		       XLit (LInt 1),
		       XLit (LStr "hello")));
	  CSwitch (XQuote "Y", CBlock [
	    CLabeled (CaseConst (LInt 1), CBlock [
	      CExpr (XQuote "CAMLparam1(X)");
	      CBreak;
	    ]);
	    CLabeled (CaseDefault, CReturn (XId "Y"))]);
	  CIf (XId "true", CBlock [], CEmpty);
	  CIf (XId "false", CBlock [], CBlock []);
	  CLabeled (Label "l1", CLabeled (Label "l2", CExpr (XQuote "Hello")));
	  CGoto "l1";
	  CFor ((`none, None, None), CEmpty);
	  CFor ((`decl (int, "i", Some (XLit (LInt 1))),
		 Some (XOp2 (O2Comp `Lt, XId "i", XLit (LInt 0))),
		 Some (XOp1 (O1Arith `PreInc, XId "i"))),
		CBlock [
		  CIf (XQuote "i", CBreak, CContinue)]);
	  CWhile (
	    CEmbedded.(
	      let x = var "x" and y = var "y" in
	      XStmtExpr [CExpr (cast int x + y + ref y + (x + sref y "blah" + sderef x "yeah"));
			 CExpr (XQuote "sadfasdf")]),
	    CEmpty);
	  CDoWhile (CWhile (XId "true", CEmpty), XId "true");
	  CExpr CEmbedded.(
	    let x = var "x" and y = var "y" and z = var "z" in
	    XStmtExpr [CExpr (cast int x * (y land ref y) + (x + sref y "blah" + sderef x "yeah"));
		       CExpr (idx x y);
		       CExpr (call "printf" [XLit (LStr "%d, %f"); XOp2 (O2Comma, x, y); y; XIIf (x, y, z)])]
	  )
	]
      ]);
      pp_code CEmbedded.(block [
	decl int "x" (intlit 0);
	for_ever [
	  expr (call "printf" [strlit "Hello world\n"]);
	  expr (call "printf" [strlit "How are we today?\n"]);
	];
	let x = "x" in
	for' int x (intlit 0) (var x < intlit 0) (inc (var x)) [
	]
      ])
    ]
  in
  Format.set_margin 40;
  pp Format.std_formatter
