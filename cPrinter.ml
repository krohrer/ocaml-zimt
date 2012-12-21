open C
open Printf
open PrettyPrinter

(* Generic------------------------------------------------------------------*)

let pp_ident n =
  pp_string n

let pp_comma = pp_string "," +++ pp_spc

(* Types -------------------------------------------------------------------*)

let pp_type_sign_spec = function
  | Unsigned	-> pp_string "unsigned" +++ pp_spc
  | Signed	-> pp_string "signed" +++ pp_spc
  | DefaultSign	-> pp_empty

let int_type_to_string = function
  | Char	-> "char"
  | Short	-> "short"
  | Int		-> "int"
  | Long	-> "long"
  | LongLong	-> "long long"

let real_type_to_string = function
  | Float	-> "float" 
  | Double 	-> "double"
  | LongDouble	-> "long double"

let type_qual_to_string = function
  | Const	-> "const"
  | Restrict	-> "restrict"
  | Volatile	-> "volatile"

let pp_type_quals pos qs =
  let pp_qual q =
    match pos with
    | `prefix	-> pp_to_string type_qual_to_string q +++ pp_spc
    | `postfix	-> pp_spc +++ pp_to_string type_qual_to_string q
  in
  pp_list ~elem:pp_qual qs

let named_type_to_string = function
  | NamedStruct name	-> sprintf "struct %s" name
  | NamedUnion name	-> sprintf "union %s" name
  | NamedEnum name	-> sprintf "enum %s" name
  | Typedef name	-> name

let pp_type_array_sizes sizes =
  let pp_size i = pp_bracket_square (if i < 0 then pp_empty else pp_int i) in
  pp_list ~elem:pp_size sizes

let rec pp_type ?(partial=pp_empty) = function
  | TVoid		-> pp_type_void partial
  | TBool qs		-> pp_type_bool qs partial
  | TInt (qs,s,it)	-> pp_type_int qs s it partial
  | TReal (qs,rt)	-> pp_type_real qs rt partial
  | TNamed (qs,nt)	-> pp_type_named qs nt partial
  | TStruct (no,sd)	-> pp_type_struct no sd partial
  | TUnion (no,ud)	-> pp_type_union no ud partial
  | TEnum (no,ed)	-> pp_type_enum no ed partial
  | TPtr (qs,t)		-> pp_type_ptr qs t partial
  | TFunc ft		-> pp_type_func ft partial
  | TArr at		-> pp_type_array at partial

and pp_type_void partial =
  pp_string "void" +++ partial

and pp_type_bool qs partial =
  pp_type_quals `prefix qs +++  pp_string "bool" +++ partial

and pp_type_int qs s it partial =
  pp_type_quals `prefix qs +++ pp_type_sign_spec s
  +++ pp_to_string int_type_to_string it +++ partial

and pp_type_real qs rt partial =
  pp_type_quals `prefix qs +++ pp_to_string real_type_to_string rt +++ partial

and pp_type_named qs nt partial =
  pp_type_quals `prefix qs +++ pp_to_string named_type_to_string nt +++ partial

and pp_type_struct no sd partial =
  let prefix =
    match no with
    | None -> pp_string "struct "
    | Some n -> pp_format "struct %s" n
  in
  prefix +++ pp_nbsp +++ pp_type_fields sd +++ partial

and pp_type_union no ud partial =
  let prefix =
    match no with
    | None -> pp_string "union "
    | Some n -> pp_format "union %s" n
  in
  prefix +++ pp_nbsp +++ pp_type_fields ud +++ partial

and pp_type_fields fs =
  let fields = 
    pp_list ~elem:pp_type_field ~sep:pp_comma fs
  in
  pp_bracket_curly (pp_spc +++ pp_vbox ~ind:0 fields +++ pp_spc)

and pp_type_field = function
  | Field (t,n)		-> pp_hbox (pp_decl t n None)
  | BitsField (t,n,sz)	-> pp_hbox (pp_decl t n None +++ pp_format " : %d" sz)
  | BitsPadding (t,sz)	-> pp_hbox (pp_type t ~partial:(pp_format " : %d" sz))

and pp_type_enum no ed partial =
  let prefix =
    match no with
    | None -> pp_string "enum "
    | Some n -> pp_format "enum %s" n
  in
  prefix +++ pp_nbsp +++ pp_type_enum_cases ed +++ partial

and pp_type_enum_cases cs =
      let cases =
	pp_list ~elem:pp_type_enum_case ~sep:pp_comma cs
      in
      pp_bracket_curly (pp_spc +++ pp_vbox ~ind:0 (cases) +++ pp_spc)
	
and pp_type_enum_case (n,xo) =
  match xo with
  | None -> pp_string n
  | Some x -> pp_hbox (pp_format "%s =" n +++ pp_spc +++ pp_comma_separated_expr x)

and pp_type_ptr qs t partial =
  let partial = pp_string "*" +++ pp_type_quals `postfix qs +++ partial in
  let partial = 
    match t with
    (* Pointers to functions and arrays need to be parenthesized *)
    | TFunc _
    | TArr _	-> pp_spc +++ pp_box ~ind:0 (pp_parenthesize partial)
    | s		-> partial
  in
  pp_type ~partial t

and pp_type_func (t,args,arity) partial =
  pp_type ~partial:(partial +++ pp_type_func_arg_list args arity) t

and pp_type_func_arg =
  function
  | t, Some n 	-> pp_box ~ind:2 (pp_decl t n None)
  | t, None 	-> pp_box ~ind:2 (pp_type t)

and pp_type_func_arg_list args arity =
  let pp_args = pp_list ~elem:pp_type_func_arg ~sep:pp_comma args in
  let pp_args =
    match arity with
    | Fixed	-> pp_args
    | Variadic	-> pp_args +++ pp_comma +++ pp_string "..."
  in
  pp_parenthesize (pp_hvbox ~ind:0 pp_args)

and pp_type_array (t,sizes) partial =
  let pp_sizes = pp_list ~elem:pp_type_array_size sizes in
  let partial = pp_box ~ind:2 (partial +++ pp_sizes) in
  pp_type ~partial t

and pp_type_array_size i =
  pp_bracket_square (if i < 0 then pp_empty else pp_int i)

(* Expressions -------------------------------------------------------------*)

(* declarations are not strictly expressions, but hey. *)
and pp_decl t name xopt =
  let ppd = pp_type ~partial:(pp_spc +++ pp_string name) t 
  and ppinit =
    match xopt with
    | None	-> pp_empty 
    | Some x	->
      pp_string " =" +++ pp_spc +++ pp_subexpr `R CExpr.assign_precedence x
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
  | XQuote s			-> pp_expr_quote s
  | XLit l			-> pp_literal l
  | XId n			-> pp_ident n
  | XCall (f,args)		-> pp_expr_call f args
  | XOp1 (op,a)			-> pp_expr_op1 (CExpr.precedence x) op a
  | XOp2 (op,a,b)		-> pp_expr_op2 (CExpr.precedence x) op a b
  | XStmtExpr stmts		-> pp_expr_stmt_expr stmts
  | XIIf (p,t,f)		-> pp_expr_inline_if (CExpr.precedence x) p t f
  | XInit is			-> pp_expr_initializer is

and pp_expr_quote s =
  pp_string s

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
  let rx = CExpr.precedence x in
  pp_paren_expr ~cond:(rx >= CExpr.comma_precedence) x

and pp_comma_separated_expr_list xs =
  pp_list ~elem:pp_comma_separated_expr ~sep:pp_comma xs

and pp_expr_arglist args =
  let pp_args = pp_comma_separated_expr_list args in
  pp_parenthesize (pp_hvbox ~ind:0 pp_args)

and pp_subexpr placement rparent x =
  let rx = CExpr.precedence x in
  if rx < rparent then
    pp_expr x
  else if rx > rparent then
    pp_paren_expr x
  else
    let assoc = CExpr.associativity rx in
    match placement, assoc with
    | `L, `R2L -> pp_paren_expr x
    | `R, `L2R -> pp_paren_expr x
    | _, _ -> pp_expr x

and pp_expr_call f args =
  pp_subexpr `L CExpr.call_precedence f +++ pp_expr_arglist args

and pp_expr_prefix r ops x =
  pp_string ops +++ pp_subexpr `R r x

and pp_expr_postfix r x ops =
  pp_subexpr `L r x +++ pp_string ops

and pp_expr_infix r x ops y =
  let pp_left	= pp_subexpr `L r x
  and pp_op	= pp_nbsp +++ pp_string ops +++ pp_spc
  and pp_right	= pp_subexpr `R r y in
  pp_left +++ pp_op +++ pp_right

and pp_expr_cast r t x =
  pp_box ~ind:1 (pp_parenthesize (pp_type t)) +++ pp_subexpr `R r x

and pp_expr_op1 r op x =
  match op with
  | O1Arith `Neg      	-> pp_expr_prefix  r "-"	x
  | O1Arith `PreInc	-> pp_expr_prefix  r "++"	x
  | O1Arith `PreDec	-> pp_expr_prefix  r "--"	x
  | O1Arith `PostInc	-> pp_expr_postfix r		x "++"
  | O1Arith `PostDec	-> pp_expr_postfix r		x "--"
  | O1Bit `Not		-> pp_expr_prefix  r "~"	x
  | O1Logic `Not	-> pp_expr_prefix  r "!"	x
  | O1Deref		-> pp_expr_prefix  r "*"	x
  | O1StructDeref f	-> pp_expr_postfix r 	x ("->"^f)
  | O1Ref		-> pp_expr_prefix  r "&" 	x
  | O1StructRef f	-> pp_expr_postfix r 	x ("."^f)
  | O1Cast t		-> pp_expr_cast r t x

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

and pp_expr_op2 r op x y =
  match op with
  | O2Arith o		-> pp_expr_infix r x (arith2_to_string o) y
  | O2Comp o		-> pp_expr_infix r x (comp2_to_string  o) y
  | O2Logic o		-> pp_expr_infix r x (logic2_to_string o) y
  | O2Bit o		-> pp_expr_infix r x (bit2_to_string   o) y
  | O2Assign		-> pp_expr_infix r x "="                  y
  | O2Subscript		-> pp_subexpr `L r x +++ pp_bracket_square (pp_expr y)
  | O2Comma		-> pp_subexpr `L r x +++ pp_comma +++ pp_subexpr `R r y

and pp_expr_stmt_expr stmts =
  let pp_stmts = pp_list ~elem:pp_code stmts in
  pp_box ~ind:3 (pp_bracket "({" "})" (pp_stmts +++ pp_brk 1 ~-3))

and pp_expr_inline_if r pred bt bf =
  let pp_pred	= pp_paren_expr ~cond:(r < CExpr.precedence pred) pred
  and pp_true	= pp_paren_expr ~cond:(r < CExpr.precedence bt  ) bt
  and pp_false	= pp_paren_expr ~cond:(r < CExpr.precedence bf  ) bf in
  pp_box ~ind:0 (pp_pred
		 +++ pp_spc +++ pp_string "? " +++ pp_true
		 +++ pp_spc +++ pp_string ": " +++ pp_false)

and pp_expr_initializer xs =
  let pp_inits = pp_comma_separated_expr_list xs in
  pp_box ~ind:2 (pp_bracket_curly (pp_spc +++ pp_inits +++ pp_spc))

(* Statements --------------------------------------------------------------*)
(* Statements include an implicit space before the actual code. This
   is so that labels can be indented back by prefixing them by the
   appropriate pp_brk statement instead of pp_spc. *)

and pp_code = function
  | CEmpty		-> pp_code_empty
  | CExpr x		-> pp_code_expr x
  | CSeq sl		-> pp_seq (List.map pp_code sl)
  | CBlock _ as s	-> pp_code_block s
  | CDecl (t,n,xopt)	-> pp_code_decl t n xopt
  | CSwitch (x,s)	-> pp_code_switch x s
  | CLabeled (l,s)	-> pp_code_labeled l +++ pp_code s
  | CGoto n		-> pp_code_goto n
  | CFor (ipn, s)	-> pp_code_for ipn s
  | CWhile (x,s)	-> pp_code_while x s
  | CDoWhile (s,x)	-> pp_code_do_while s x
  | CIf (xp,st,sf)	-> pp_code_if xp st sf
  | CBreak		-> pp_code_break ()
  | CContinue		-> pp_code_continue ()
  | CReturn x		-> pp_code_return x

and pp_code_empty =
  pp_empty

(* As it stands, pp_bracket_stmt has the closing bracket \} always on
   a new line. *)
and pp_code_block ?prefix ?postfix s =
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
    | CEmpty		-> pp_empty
    | CBlock sts	-> pp_list ~elem:pp_code sts
    | s			-> pp_code s
  in
  pp_spc +++ pp_vbox ~ind:4 (ppo +++ ppb +++ pp_brk 0 ~-4 +++ ppc)

and pp_code_statement pp =
  pp_spc +++ pp_box ~ind:2 (pp +++ pp_string ";")
    

and pp_code_expr x =
  pp_code_statement (pp_expr x);

and pp_code_decl t name xopt =
  pp_code_statement (pp_decl t name xopt)


and pp_code_labeled l =
  let ind, pp_lbl = 
    match l with
    | CaseConst l	-> ~-4, pp_string "case " +++ pp_literal l
    | CaseNamed n	-> ~-4, pp_string "case " +++ pp_string n
    | CaseDefault	-> ~-4, pp_string "default"
    | Label n		-> ~-2, pp_string n
  in
  pp_brk 1 ind +++ pp_lbl +++ pp_string ":"

and pp_code_goto n =
  pp_code_statement (pp_string "goto " +++ pp_string n)


and pp_code_switch x s =
  let prefix = pp_string "switch " +++ pp_paren_expr x in
  pp_code_block ~prefix s

and pp_code_for ipn s =
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
  pp_code_block ~prefix s

and pp_code_while x s =
  let prefix = pp_string "while " +++ pp_paren_expr x in
  pp_code_block ~prefix s

and pp_code_do_while s x =
  let prefix = pp_string "do"
  and postfix = pp_string "while " +++ pp_paren_expr x in
  pp_code_block ~prefix ~postfix s

and pp_code_if xpred strue sfalse =
  let pptrue = 
    pp_code_block ~prefix:(pp_string "if " +++ pp_paren_expr xpred) strue
  and ppfalse =
    match sfalse with
    | CEmpty	-> pp_empty
    | s		-> pp_code_block ~prefix:(pp_string "else") s
  in
  pptrue +++ ppfalse


and pp_code_break () =
  pp_code_statement (pp_string "break")

and pp_code_continue () =
  pp_code_statement (pp_string "continue")

and pp_code_return x =
  pp_code_statement (pp_string "return " +++ pp_paren_expr x)
