open C
open CLang

type pp = Format.formatter -> unit

(* General -----------------------------------------------------------------*)

let std_indent = 2

let pp_empty ff = ()
let (+++) f g = fun ff -> f ff; g ff
let ( *** ) f x = f x

(* f +++ pp_emtpy === pp_empty +++ f *)
(* (f +++ g) +++ h === f +++ (g +++ h) *)

let pp_string s ff = Format.pp_print_string ff s
let pp_int i ff = Format.pp_print_int ff i
let pp_spc ff = Format.pp_print_space ff ()

let pp_format fmt =
  Format.ksprintf (fun s ff -> Format.pp_print_string ff s) fmt

let pp_to_string f x ff = Format.pp_print_string ff (f x)
let pp_nbsp ff = Format.pp_print_string ff " "

let pp_list ~elem ?(sep=pp_empty) list ff =
  let rec fold = function
    | []	-> ()
    | [x]	-> elem x ff
    | x::rest	-> elem x ff; sep ff; fold rest
  in
  fold list

let pp_seq = List.fold_left (+++) pp_empty

let pp_bracket sopen sclose pp = pp_string sopen +++ pp +++ pp_string sclose
let pp_parenthesize pp = pp_bracket "(" ")" pp
let pp_bracket_curly pp = pp_bracket "{" "}" pp
let pp_bracket_square pp = pp_bracket "[" "]" pp

let pp_comma = pp_string "," +++ pp_spc

let pp_box ~ind pp ff =
  Format.pp_open_box ff ind;
  pp ff;
  Format.pp_close_box ff ()
  
let pp_hbox pp ff =
  Format.pp_open_hbox ff ();
  pp ff;
  Format.pp_close_box ff ()

let pp_vbox ~ind pp ff =
  Format.pp_open_vbox ff ind;
  pp ff;
  Format.pp_close_box ff ()

let pp_hvbox ~ind pp ff =
  Format.pp_open_hvbox ff ind;
  pp ff;
  Format.pp_close_box ff ()

let pp_cut ff = Format.pp_print_cut ff ()
let pp_brk nsp ind ff = Format.pp_print_break ff nsp ind

(* Types -------------------------------------------------------------------*)

let pp_sign_spec = function
  | `unsigned	-> pp_string "unsigned" +++ pp_spc
  | `signed	-> pp_string "signed" +++ pp_spc
  | `default	-> pp_empty

let int_type_to_string = function
  | `char	-> "char"
  | `short	-> "short"
  | `int	-> "int"
  | `long	-> "long"
  | `longlong	-> "long long"

let real_type_to_string = function
  | `float	-> "float" 
  | `double 	-> "double"
  | `longdouble	-> "long double"

let pp_prim_type = function
  | PBool	-> pp_string "bool"
  | PInt (s,i)	-> pp_sign_spec s +++ pp_to_string int_type_to_string i
  | PReal rt	-> pp_to_string real_type_to_string rt

let type_qual_to_string = function
  | `const	-> "const"
  | `restrict	-> "restrict"
  | `volatile	-> "volatile"

let pp_type_quals pos qs =
  let pp_qual q =
    match pos with
    | `prefix	-> pp_to_string type_qual_to_string q +++ pp_spc
    | `postfix	-> pp_spc +++ pp_to_string type_qual_to_string q
  in
  pp_list ~elem:pp_qual qs

let pp_ref_type = function
  | `struct', name	-> pp_format "struct %s" name
  | `union, name	-> pp_format "union %s" name
  | `enum, name		-> pp_format "enum %s" name
  | `typedef, name	-> pp_string name

let pp_array_sizes sizes =
  let pp_size i = pp_bracket_square (if i < 0 then pp_empty else pp_int i) in
  pp_list ~elem:pp_size sizes

let rec pp_type ?(partial=pp_empty) = function
  | TVoid		-> pp_string "void" +++ partial
  | TPrim (qs,p)	-> pp_prim qs p partial
  | TRef (qs,r)		-> pp_ref qs r partial
  | TPtr (qs,t)		-> pp_ptr qs t partial
  | TFunc ft		-> pp_func ft partial
  | TArr at		-> pp_arr at partial

and pp_prim qs p partial =
  pp_type_quals `prefix qs +++ pp_prim_type p +++ partial

and pp_ref qs r partial =
  pp_type_quals `prefix qs +++ pp_ref_type r +++ partial

and pp_ptr qs t partial =
  let partial = pp_string "*" +++ pp_type_quals `postfix qs +++ partial in
  let partial = 
    match t with
    (* Pointers to functions and arrays need to be parenthesized *)
    | TFunc _
    | TArr _	-> pp_spc +++ pp_box ~ind:0 (pp_parenthesize partial)
    | s		-> partial
  in
  pp_type ~partial t

and pp_func (t,args,arity) partial =
  pp_type ~partial:(partial +++ pp_arg_list args arity) t

and pp_arg t =
  pp_box ~ind:std_indent (pp_type t)

and pp_arg_list args arity =
  let pp_args = pp_list ~elem:pp_arg ~sep:pp_comma args in
  let pp_args =
    match arity with
    | `fixed	-> pp_args
    | `variadic	-> pp_args +++ pp_comma +++ pp_string "..."
  in
  pp_parenthesize (pp_hvbox ~ind:0 pp_args)

and pp_arr (t,sizes) partial =
  let pp_sizes = pp_list ~elem:pp_array_size sizes in
  let partial = pp_box ~ind:std_indent (partial +++ pp_sizes) in
  pp_type ~partial t

and pp_array_size i =
  pp_bracket_square (if i < 0 then pp_empty else pp_int i)

(* Expressions -------------------------------------------------------------*)

(* declarations are not strictly expressions, but hey. *)
let rec pp_decl t name xopt =
  let ppd = pp_type ~partial:(pp_spc +++ pp_string name) t 
  and ppinit =
    match xopt with
    | None	-> pp_empty 
    | Some x	->
      pp_string " =" +++ pp_spc +++ pp_subexpr `R Expr.assign_precedence x
  in
  ppd +++ ppinit

and pp_paren_expr ?(cond=true) x =
  let ppe = pp_expr x in
  if cond then
    pp_box ~ind:1 (pp_parenthesize ppe)
  else 
    ppe

and pp_expr x =
  match x with
  | XQuote s			-> pp_string s
  | XLit l			-> pp_literal l
  | XId n			-> pp_string n
  | XCall (f,args)		-> pp_call f args
  | XOp1 (op,a)			-> pp_op1 (Expr.precedence x) op a
  | XOp2 (op,a,b)		-> pp_op2 (Expr.precedence x) op a b
  | XStmtExpr stmts		-> pp_stmt_expr stmts
  | XIIf (p,t,f)		-> pp_inline_if (Expr.precedence x) p t f
  | XInit is			-> pp_initializer is

and pp_literal = function
  | LInt i	-> pp_format "%d" i
  | LInt32 i	-> pp_format "%ld" i
  | LInt64 i	-> pp_format "%LdLL" i
  | LUInt u	-> pp_format "%uU" u
  | LUInt32 u	-> pp_format "%luU" u
  | LUInt64 u 	-> pp_format "%LuULL" u
  | LStr s	-> pp_format "%S" s
  (* A bit more precision for floats than actually present, to ensure
     that we have the same representation on the other end. *)
  | LFloat32 f	-> pp_format "%.9gF" f
  | LFloat64 f	-> pp_format "%.18g" f

and pp_comma_separated_expr x =
  let rx = Expr.precedence x in
  pp_paren_expr ~cond:(rx >= Expr.comma_precedence) x

and pp_comma_separated_expr_list xs =
  pp_list ~elem:pp_comma_separated_expr ~sep:pp_comma xs

and pp_arglist args =
  let pp_args = pp_comma_separated_expr_list args in
  pp_parenthesize (pp_hvbox ~ind:0 pp_args)

and pp_subexpr placement rparent x =
  let rx = Expr.precedence x in
  if rx < rparent then
    pp_expr x
  else if rx > rparent then
    pp_paren_expr x
  else
    let assoc = Expr.associativity rx in
    match placement, assoc with
    | `L, `R2L -> pp_paren_expr x
    | `R, `L2R -> pp_paren_expr x
    | _, _ -> pp_expr x

and pp_call f args =
  pp_subexpr `L Expr.call_precedence f +++ pp_arglist args

and pp_prefix r ops x =
  pp_string ops +++ pp_subexpr `R r x

and pp_postfix r x ops =
  pp_subexpr `L r x +++ pp_string ops

and pp_infix r x ops y =
  let pp_left	= pp_subexpr `L r x
  and pp_op	= pp_nbsp +++ pp_string ops +++ pp_spc
  and pp_right	= pp_subexpr `R r y in
  pp_left +++ pp_op +++ pp_right

and pp_cast r t x =
  pp_box ~ind:1 (pp_parenthesize (pp_type t)) +++ pp_subexpr `R r x

and pp_op1 r op x =
  match op with
  | Op1Arith `Neg      	-> pp_prefix  r "-"	x
  | Op1Arith `PreInc	-> pp_prefix  r "++"	x
  | Op1Arith `PreDec	-> pp_prefix  r "--"	x
  | Op1Arith `PostInc	-> pp_postfix r		x "++"
  | Op1Arith `PostDec	-> pp_postfix r		x "--"
  | Op1Bit `Not		-> pp_prefix  r "~"	x
  | Op1Logic `Not	-> pp_prefix  r "!"	x
  | Op1Deref		-> pp_prefix  r "*"	x
  | Op1StructDeref f	-> pp_postfix r 	x ("->"^f)
  | Op1Ref		-> pp_prefix  r "&" 	x
  | Op1StructRef f	-> pp_postfix r 	x ("."^f)
  | Op1Cast t		-> pp_cast r t x

and arith2_to_string = function
  | `Add	-> "+"
  | `Sub	-> "-"
  | `Mul	-> "*"
  | `Div	-> "/"
  | `Mod	-> "%"

and comp2_to_string = function
  | `Eq		-> "=="
  | `NE		-> "!="
  | `Gt		-> ">"
  | `Lt		-> "<"
  | `GE		-> ">="
  | `LE		-> "<="

and logic2_to_string = function
  | `And	-> "&&"
  | `Or		-> "||"

and bit2_to_string = function
  | `And	-> "&"
  | `Or		-> "|"
  | `Xor	-> "^"
  | `ShiftL	-> "<<"
  | `ShiftR	-> ">>"

and pp_op2 r op x y =
  match op with
  | Op2Arith o		-> pp_infix r x (arith2_to_string o) y
  | Op2Comp o		-> pp_infix r x (comp2_to_string  o) y
  | Op2Logic o		-> pp_infix r x (logic2_to_string o) y
  | Op2Bit o		-> pp_infix r x (bit2_to_string   o) y
  | Op2Assign		-> pp_infix r x "="                  y
  | Op2Subscript	-> pp_subexpr `L r x +++ pp_bracket_square (pp_expr y)
  | Op2Comma		-> pp_subexpr `L r x +++ pp_comma +++ pp_subexpr `R r y

and pp_stmt_expr stmts =
  let pp_stmts = pp_list ~elem:pp_stmt stmts in
  pp_box ~ind:3 (pp_bracket "({" "})" (pp_stmts +++ pp_brk 1 ~-3))

and pp_inline_if r pred bt bf =
  let pp_pred	= pp_paren_expr ~cond:(r < Expr.precedence pred) pred
  and pp_true	= pp_paren_expr ~cond:(r < Expr.precedence bt  ) bt
  and pp_false	= pp_paren_expr ~cond:(r < Expr.precedence bf  ) bf in
  pp_box ~ind:0 (pp_pred
		 +++ pp_spc +++ pp_string "? " +++ pp_true
		 +++ pp_spc +++ pp_string ": " +++ pp_false)

and pp_initializer xs =
  let pp_inits = pp_comma_separated_expr_list xs in
  pp_box ~ind:2 (pp_bracket_curly (pp_spc +++ pp_inits +++ pp_spc))

(* Statements --------------------------------------------------------------*)
(* Statements include an implicit space before the actual code. This
   is so that labels can be indented back by prefixing them by the
   appropriate pp_brk statement instead of pp_spc. *)

and pp_stmt = function
  | SEmpty		-> pp_empty
  | SExpr x		-> pp_expr_stmt x
  | SSeq sl		-> pp_seq (List.map pp_stmt sl)
  | SBlock _ as s	-> pp_bracket_stmt s
  | SDecl (t,n,xopt)	-> pp_decl_stmt t n xopt
  | SSwitch (x,s)	-> pp_switch x s
  | SLabeled (l,s)	-> pp_label l +++ pp_stmt s
  | SGoto n		-> pp_goto n
  | SFor (ipn, s)	-> pp_for ipn s
  | SWhile (x,s)	-> pp_while x s
  | SDoWhile (s,x)	-> pp_do_while s x
  | SIf (xp,st,sf)	-> pp_if xp st sf
  | SBreak		-> pp_break ()
  | SContinue		-> pp_continue ()
  | SReturn x		-> pp_return x

(* As it stands, pp_bracket_stmt has the closing bracket \} always on
   a new line. *)
and pp_bracket_stmt ?prefix ?postfix s =
  let ppo = 
    match prefix with
    | None		-> pp_string "{"
    | Some pp		-> pp_hbox (pp +++ pp_string " {")
  and ppc =
    match postfix with
    | None		-> pp_string "}"
    | Some pp		-> pp_hbox (pp_string "} " +++ pp)
  and ppb =
    match s with
    | SEmpty		-> pp_empty
    | SBlock sts	-> pp_list ~elem:pp_stmt sts
    | s			-> pp_stmt s
  in
  pp_spc +++ pp_vbox ~ind:4 (ppo +++ ppb +++ pp_brk 0 ~-4 +++ ppc)

and pp_single_stmt pp =
  pp_spc +++ pp_box ~ind:2 (pp +++ pp_string ";")
    

and pp_expr_stmt x =
  pp_single_stmt (pp_expr x);

and pp_decl_stmt t name xopt =
  pp_single_stmt (pp_decl t name xopt)


and pp_label l =
  let ind, pp_lbl = 
    match l with
    | CaseConst l	-> ~-4, pp_string "case " +++ pp_literal l
    | CaseNamed n	-> ~-4, pp_string "case " +++ pp_string n
    | CaseDefault	-> ~-4, pp_string "default"
    | Label n		-> ~-2, pp_string n
  in
  pp_brk 1 ind +++ pp_lbl +++ pp_string ":"

and pp_goto n =
  pp_single_stmt (pp_string "goto " +++ pp_string n)


and pp_switch x s =
  let prefix = pp_string "switch " +++ pp_paren_expr x in
  pp_bracket_stmt ~prefix s

and pp_for ipn s =
  let pph = 
    match ipn with
    | `none, None, None	-> pp_string ";;"
    | init, pred, next	->
      let ppinit =
	match init with
	| `none			-> pp_empty
	| `decl (t, name, xopt)	-> pp_decl t name xopt
	| `expr x		-> pp_expr x
      and pppred = 
	match pred with
	| None		-> pp_spc
	| Some x	-> pp_spc +++ pp_expr x
      and ppnext =
	match next with
	| None		-> pp_spc
	| Some x	-> pp_spc +++ pp_expr x
      and ppsep = pp_string ";" in
      ppinit +++ ppsep +++ pppred +++ ppsep +++ ppnext
  in
  let prefix = pp_string "for (" +++ pp_hvbox ~ind:0 pph +++ pp_string ")" in
  pp_bracket_stmt ~prefix s

and pp_while x s =
  let prefix = pp_string "while " +++ pp_paren_expr x in
  pp_bracket_stmt ~prefix s

and pp_do_while s x =
  let prefix = pp_string "do"
  and postfix = pp_string "while " +++ pp_paren_expr x in
  pp_bracket_stmt ~prefix ~postfix s

and pp_if xpred strue sfalse =
  let pptrue = 
    pp_bracket_stmt ~prefix:(pp_string "if " +++ pp_paren_expr xpred) strue
  and ppfalse =
    match sfalse with
    | SEmpty	-> pp_empty
    | s		-> pp_bracket_stmt ~prefix:(pp_string "else") s
  in
  pptrue +++ ppfalse


and pp_break () =
  pp_single_stmt (pp_string "break")

and pp_continue () =
  pp_single_stmt (pp_string "continue")

and pp_return x =
  pp_single_stmt (pp_string "return " +++ pp_paren_expr x)
