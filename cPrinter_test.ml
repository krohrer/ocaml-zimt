open C
open PrettyPrinter
open CPrinter

let int		= TInt ([],DefaultSign,Int)
let void	= TVoid
let vptr	= TPtr ([Const], TVoid)
let func	= TFunc (void, [void, None], Fixed)
let fptr	= TPtr ([Const], TFunc (void, [void, None], Fixed))
let fpa		= TArr (fptr, [-1])
let mfp f	= TPtr ([Const], TFunc (f, [int, Some "a"; int, Some "b"], Fixed))
let s		= TStruct (None, [])
let s2		= TStruct (Some "S2", [Field (fptr, "a");
				       BitsField (int, "b", 4);
				       BitsPadding (int,18);
				       Field (s, "c")])

let ptr t = TPtr ([], t)
(*let func t args = TFunc (t, args, Fixed) *)
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
  let pp_lines ls = pp_vbox ~ind:0 (pp_list ~elem:(fun l -> pp_hbox l +++ pp_spc) ls) in
  let pp = 
    pp_lines [
      pp_typedef (int, "a");
      pp_typedef (void, "a");
      pp_typedef (vptr, "a");
      pp_typedef (func, "a");
      pp_typedef (fptr, "a");
      pp_typedef (fpa, "a");
      pp_typedef (s, "bas");
      pp_typedef (s2, "s2");
      pp_defvar ((None,func,"f1"), XLit (LQuote "NULL"));
      pp_defunc ((None,func,"bar1"), CExpr (XLit (LQuote "NULL")));
      pp_defvar ((None,func,"f2"), XLit (LQuote "NULL"));
      pp_defunc ((None,TFunc (void, [void, Some "x"], Variadic),"bar2"), CSeq [
	CDef ((None,fptr,"f"), XLit (LInt 0));
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
	  CFor ((`def ((None,int,"x"),XLit (LInt 0)), None, None), CEmpty);
	  CFor ((`def ((None,int,"i"), XLit (LInt 1)),
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
  Format.set_margin 32;
  pp Format.std_formatter
