open C_UntypedAST
open C_Untyped

type pp = Format.formatter -> unit

(* General -----------------------------------------------------------------*)

let std_indent = 2

(* Pretty printers form a monoid *)
let pp_empty ff = ()
let (+++) f g = fun ff -> f ff; g ff
let ( *** ) f x = f x

(* f +++ pp_emtpy === pp_empty +++ f *)
(* (f +++ g) +++ h === f +++ (g +++ h) *)

let pp_string s ff = Format.pp_print_string ff s
let pp_int i ff = Format.pp_print_int ff i
let pp_space ff = Format.pp_print_space ff ()

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

let pp_comma = pp_string "," +++ pp_space

let pp_box ~indent pp ff =
  Format.pp_open_box ff indent;
  pp ff;
  Format.pp_close_box ff ()
  
let pp_hbox pp ff =
  Format.pp_open_hbox ff ();
  pp ff;
  Format.pp_close_box ff ()

let pp_vbox ~indent pp ff =
  Format.pp_open_vbox ff indent;
  pp ff;
  Format.pp_close_box ff ()

let pp_hvbox ~indent pp ff =
  Format.pp_open_hvbox ff indent;
  pp ff;
  Format.pp_close_box ff ()

let pp_cut ff = Format.pp_print_cut ff ()
let pp_break nsp ind ff = Format.pp_print_break ff nsp ind

(* Types -------------------------------------------------------------------*)

let pp_sign_spec = function
  | `unsigned	-> pp_string "unsigned" +++ pp_space
  | `signed	-> pp_string "signed" +++ pp_space
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
    | `prefix	-> pp_to_string type_qual_to_string q +++ pp_space
    | `postfix	-> pp_space +++ pp_to_string type_qual_to_string q
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
    | TArr _	-> pp_space +++ pp_box ~indent:0 (pp_parenthesize partial)
    | s		-> partial
  in
  pp_type ~partial t

and pp_func (t,args,arity) partial =
  pp_type ~partial:(partial +++ pp_arg_list args arity) t

and pp_arg t =
  pp_box ~indent:std_indent (pp_type t)

and pp_arg_list args arity =
  let pp_args = pp_list ~elem:pp_arg ~sep:pp_comma args in
  let pp_args =
    match arity with
    | `fixed	-> pp_args
    | `variadic	-> pp_args +++ pp_comma +++ pp_string "..."
  in
  pp_parenthesize (pp_hvbox ~indent:0 pp_args)

and pp_arr (t,sizes) partial =
  let pp_sizes = pp_list ~elem:pp_array_size sizes in
  let partial = pp_box ~indent:std_indent (partial +++ pp_sizes) in
  pp_type ~partial t

and pp_array_size i =
  pp_bracket_square (if i < 0 then pp_empty else pp_int i)

let pp_decl t name =
  pp_hbox (pp_type ~partial:(pp_space +++ pp_string name) t)

(* Expressions -------------------------------------------------------------*)

let pp_paren_expr ?(cond=true) pp =
  if cond then
    pp_box ~indent:1 (pp_parenthesize pp)
  else 
    pp

let rec pp_expr x =
  match x with
  | XQuote s			-> pp_string s
  | XLit l			-> pp_literal l
  | XIdent n			-> pp_string n
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
  let pp_x = pp_expr x in
  pp_paren_expr ~cond:(rx < Expr.comma_precedence) pp_x

and pp_comma_separated_expr_list xs =
  pp_list ~elem:pp_comma_separated_expr ~sep:pp_comma xs

and pp_arglist args =
  let pp_args = 
    if args = [] then
      pp_string "void"
    else
      pp_comma_separated_expr_list args
  in
  pp_parenthesize (pp_hvbox ~indent:0 pp_args)

and pp_subexpr placement rparent x =
  let rx = Expr.precedence x in
  let pp_x = pp_expr x in
  if rx < rparent then
    pp_x
  else if rx > rparent then
    pp_paren_expr pp_x
  else
    let assoc = Expr.associativity rx in
    match placement, assoc with
    | `L, `R2L -> pp_paren_expr pp_x
    | `R, `L2R -> pp_paren_expr pp_x
    | _, _ -> pp_x

and pp_call f args =
  pp_subexpr `L Expr.call_precedence f +++ pp_arglist args

and pp_prefix r ops x =
  pp_string ops +++ pp_subexpr `R r x

and pp_postfix r x ops =
  pp_subexpr `L r x +++ pp_string ops

and pp_infix r x ops y =
  let pp_left	= pp_subexpr `L r x
  and pp_op	= pp_nbsp +++ pp_string ops +++ pp_space
  and pp_right	= pp_subexpr `R r y in
  pp_left +++ pp_op +++ pp_right

and pp_op1 r op x =
  match op with
  | Op1Arith `Neg      	-> pp_prefix  r "-"	x
  | Op1Arith `PreInc	-> pp_prefix  r "++"	x
  | Op1Arith `PreDec	-> pp_prefix  r "--"	x
  | Op1Arith `PostInc	-> pp_postfix r		x "++"
  | Op1Arith `PostDec	-> pp_postfix r		x "--"
  | Op1Bit `Not		-> pp_prefix  r "~"	x
  | Op1Logic `Not	-> pp_prefix  r "!"	x
  | Op1Cast t		-> pp_paren_expr (pp_type t) +++ pp_subexpr `R r x
  | Op1Deref		-> pp_prefix  r "*"	x
  | Op1StructDeref f	-> pp_postfix r 	x ("->"^f)
  | Op1Ref		-> pp_prefix  r "&" 	x
  | Op1StructRef f	-> pp_postfix r 	x ("."^f)

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
  | Op2Subscript	-> pp_subexpr `L r x +++ pp_bracket_square (pp_expr x)
  | Op2Comma		-> pp_subexpr `L r x +++ pp_comma +++ pp_subexpr `R r y

and pp_stmt_expr stmts =
  let pp_stmts = pp_list ~elem:pp_stmt ~sep:pp_space stmts in
  pp_hvbox ~indent:3 (pp_bracket "({" "})" (pp_space +++ pp_stmts +++ pp_space))

and pp_inline_if r pred bt bf =
  let pp_pred	= pp_paren_expr ~cond:(r < Expr.precedence pred) (pp_expr pred)
  and pp_true	= pp_paren_expr ~cond:(r < Expr.precedence bt  ) (pp_expr bt  )
  and pp_false	= pp_paren_expr ~cond:(r < Expr.precedence bf  ) (pp_expr bf  ) in
  pp_box ~indent:0 (pp_pred
		    +++ pp_space +++ pp_string "? " +++ pp_true
		    +++ pp_space +++ pp_string ": " +++ pp_false)

and pp_initializer xs =
  let pp_inits = pp_comma_separated_expr_list xs in
  pp_box ~indent:2 (pp_bracket_curly (pp_space +++ pp_inits +++ pp_space))

(* Statements --------------------------------------------------------------*)

and pp_stmt = function
  | StEmpty				-> pp_empty
  | StExpr x				-> pp_st_expr x
  | StBlock sts				-> pp_st_block sts
  | StDecl (t,n,xopt)			-> pp_st_decl t n xopt
  | StSwitch (x,st)			-> pp_st_switch x st
  | StCase c				-> pp_st_case c
  | StLabel n				-> pp_st_label n
  | StGoto n				-> pp_st_goto n
  | StFor (xinit,xpred,xnext,st)	-> pp_st_for xinit xpred xnext st
  | StWhile (x,st)			-> pp_st_while x st
  | StDoWhile (st,x)			-> pp_st_do_while st x
  | StIf (xpred,sttrue,stfalse)		-> pp_st_if xpred sttrue stfalse
  | StBreak				-> pp_st_break
  | StContinue				-> pp_st_continue
  | StReturn x				-> pp_st_return x
    
and pp_st_expr x =
  failwith "TODO"

and pp_st_block sts =
  failwith "TODO"

and pp_st_decl t n xopt =
  failwith "TODO"

and pp_st_switch x st =
  failwith "TODO"

and pp_st_case = function
  | `lit l	-> failwith "TODO"
  | `ident n	-> failwith "TODO"
  | `default	-> failwith "TODO"

and pp_st_label n =
  failwith "TODO"

and pp_st_goto n =
  failwith "TODO"

and pp_st_for xinit xpred xnext st =
  failwith "TODO"

and pp_st_while x st =
  failwith "TODO"

and pp_st_do_while st x =
  failwith "TODO"

and pp_st_if xpred sttrue stfalse =
  failwith "TODO"

and pp_st_break =
  failwith "TODO"

and pp_st_continue =
  failwith "TODO"

and pp_st_return x =
  failwith "TODO"
