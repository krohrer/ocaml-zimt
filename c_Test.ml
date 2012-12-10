open C_UntypedAST
open C_Printer

let int		= TPrim ([],PInt (`default,`int))
let void	= TVoid
let vptr	= TPtr ([`const], TVoid)
let fptr	= TPtr ([`const], TFunc (void, [void], `fixed))
let fpa		= TArr (fptr, [-1])
let mfp f	= TPtr ([`const], TFunc (void, [void; void], `fixed))

let int = TPrim ([],PInt (`default,`int))
let void = TVoid
let ptr t = TPtr ([], t)
let func t args = TFunc (t, args, `fixed) 
let arr t sizes = TArr (t, sizes)
let add_const qs = if List.mem `const qs then qs else `const :: qs
let const = function
  | TPrim (qs,p)	-> TPrim (add_const qs,p)
  | TRef (qs,r)		-> TRef (add_const qs,r)
  | TPtr (qs,p)		-> TPtr (add_const qs,p)
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
      pp_stmt (SBlock [
	SDecl (fptr, "f", None);
	SBlock [
	  SExpr (XIIf (XQuote "X",
		       XLit (LInt 1),
		       XLit (LStr "hello")));
	  SSwitch (XQuote "Y", SBlock [
	    SLabeled (CaseConst (LInt 1), SBlock [
	      SExpr (XQuote "CAMLparam1(X)");
	      SBreak;
	    ]);
	    SLabeled (CaseDefault, SReturn (XId "Y"))]);
	  SIf (XId "true", SBlock [], SEmpty);
	  SIf (XId "false", SBlock [], SBlock []);
	  SLabeled (Label "l1", SLabeled (Label "l2", SExpr (XQuote "Hello")));
	  SGoto "l1";
	  SFor ((`none, None, None), SEmpty);
	  SFor ((`decl (int, "i", Some (XLit (LInt 1))),
		 Some (XOp2 (Op2Comp `Lt, XId "i", XLit (LInt 0))),
		 Some (XOp1 (Op1Arith `PreInc, XId "i"))),
		SBlock [
		  SIf (XQuote "i", SBreak, SContinue)]);
	  SWhile (
	    C_Untyped.Embedded.(
	      let x = var "x" and y = var "y" in
	      XStmtExpr [SExpr (cast int x + y + ref y + (x + sref y "blah" + sderef x "yeah"));
			 SExpr (XQuote "sadfasdf")]),
	    SEmpty);
	  SDoWhile (SWhile (XId "true", SEmpty), XId "true");
	  SExpr C_Untyped.Embedded.(
	    let x = var "x" and y = var "y" and z = var "z" in
	    XStmtExpr [SExpr (cast int x * (y land ref y) + (x + sref y "blah" + sderef x "yeah"));
		       SExpr (idx x y);
		       SExpr (call "printf" [XLit (LStr "%d, %f"); XOp2 (Op2Comma, x, y); y; XIIf (x, y, z)])]
	  )
	]
      ]);
      pp_stmt C_Untyped.Embedded.(block [
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
