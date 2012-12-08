open C_UntypedAST
open C_Printer

let int		= TPrim ([],PInt (`default,`int))
let void	= TVoid
let vptr	= TPtr ([`const], TVoid)
let fptr	= TPtr ([`const], TFunc (void, [], `fixed))
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

let pp_nbsp_opt ~spc pp =
  if spc then pp_nbsp +++ pp else pp

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
    ]
  in
  Format.set_margin 40;
  pp Format.std_formatter
