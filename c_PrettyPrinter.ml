open C_SyntaxUntyped
open Printf
module F = Format

type formatter = F.formatter

let type_to_string t =
  "const struct XYZ*"

(* Precedence is used to decide where to insert parentheses, so that
the pretty-printed output is equivalent to the AST representation we
have here *)

type precedence = int

let precedence_to_associativity = function
  | x when x < 3	-> `L2R
  | x when x < 4	-> `R2L
  | x when x < 15	-> `L2R
  | x when x < 17	-> `R2L
  | _			-> `L2R

(* http://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence *)

let rec expr_precedence = function
  (* These are either atomic, or properly bracketed. *)
  | XIdent _
  | XStmtExpr _
  | XInit _
  | XDInit _		-> 0
  | XCall _		-> call_precedence
  | XLit l		-> lit_precedence l
  | XOp1 (o,_)		-> op1_precedence o
  | XOp2 (o,_,_)	-> op2_precedence o
  | XIIf (_,_,_)	-> 15
  (* We don't know what the quoted string contains, so we simply give
  it a very low precedence *)
  | XQuote s		-> 999999
and call_precedence = 2
and lit_precedence = function
  (* Negative numbers have the same precedence as Op1Arith `Neg *)
  | LInt i	when i < 0	-> 3
  | LInt32 i	when i < 0l	-> 3
  | LInt64 i	when i < 0L	-> 3
  | LFloat32 f	when f < 0.0	-> 3
  | LFloat64 f	when f < 0.0	-> 3 
  (* The rest is atomic *)
  | _				-> 0
and op1_precedence = function
  | Op1Arith `PostInc
  | Op1Arith `PostDec
  | Op1SDeref _
  | Op1SRef _		-> 2
  | Op1Arith `Neg
  | Op1Arith `PreInc
  | Op1Arith `PreDec
  | Op1Bit `Not
  | Op1Logic `Not
  | Op1Cast _
  | Op1Deref
  | Op1Ref		-> 3
and op2_precedence = function
  | Op2ArrSubs 		-> 2
  | Op2Arith `Mul
  | Op2Arith `Div
  | Op2Arith `Mod	-> 5
  | Op2Arith `Add
  | Op2Arith `Sub	-> 6
  | Op2Bit `Shl
  | Op2Bit `Shr		-> 7
  | Op2Comp `Gt
  | Op2Comp `Lt
  | Op2Comp `GE
  | Op2Comp `LE		-> 8
  | Op2Comp `Eq
  | Op2Comp `NE		-> 9
  | Op2Bit `And		-> 10
  | Op2Bit `Xor		-> 11
  | Op2Bit `Or		-> 12
  | Op2Logic `And	-> 13
  | Op2Logic `Or	-> 14
  | Op2Assign	 	-> 15
  | Op2Comma		-> comma_precedence
and comma_precedence = 17

let format_atom s ff =
  F.pp_print_string ff s

let format_atomf ff =
  ksprintf (format_atom) ff

(* TODO : Add box *)
let format_parenthesize n ff =
  F.pp_print_string ff "(";
  n ff;
  F.pp_print_string ff ")"

(* TODO : Add box *)
let format_curly_bracket n ff =
  F.pp_print_string ff "{";
  n ff;
  F.pp_print_string ff "}"

(* TODO : Add box *)
let format_square_bracket n ff =
  F.pp_print_string ff "[";
  n ff;
  F.pp_print_string ff "]"

let format_seq ns ff =
  List.iter (fun n -> n ff) ns

let format_list sep ns ff =
  let rec iter = function
    | []	-> ()
    | n::[]	-> n ff
    | n::rest	-> 
	n ff;
	F.pp_print_string ff sep;
	F.pp_print_space ff ();
	iter rest
  in
  iter ns

let rec format_expr x =
  match x with
  | XQuote s			-> format_atom s
  | XLit l			-> format_literal l
  | XIdent n			-> format_atom n
  | XCall (f, args)		-> format_call f args
  | XOp1 (op,a)			-> format_op1 (expr_precedence x) op a
  | XOp2 (op,a,b)		-> format_op2 (expr_precedence x) op a b
  | XStmtExpr stmts		-> format_stmt_expr stmts
  | XIIf (p,t,f)		-> format_inline_if (expr_precedence x) p t f
  | XInit xs			-> format_initializer xs
  | XDInit xs			-> format_designated_initializer xs

and format_literal = function
  | LInt i	-> format_atomf "%d" i
  | LInt32 i	-> format_atomf "%ld" i
  | LInt64 i	-> format_atomf "%LdLL" i
  | LUInt u	-> format_atomf "%uU" u
  | LUInt32 u	-> format_atomf "%luU" u
  | LUInt64 u 	-> format_atomf "%LuULL" u
  | LFloat32 f	-> format_atomf "%.9gF" f (* A bit more precision than actually present *)
  | LFloat64 f	-> format_atomf "%.18g" f (* Same here *)
  | LStr s	-> format_atomf "%S" s

and format_comma_separated_element x =
  let p_x = expr_precedence x in
  let x' = format_expr x in
  if p_x < comma_precedence then
    x'
  else
    format_parenthesize x'

and format_arglist args =
  let ns = List.map format_comma_separated_element args in
  format_parenthesize (format_list "," ns)

and format_subexpr placement p_parent x =
  let p_x = expr_precedence x in
  let nx = format_expr x in
  if p_x < p_parent then
    nx
  else if p_x > p_parent then
    format_parenthesize nx
  else
    let assoc = precedence_to_associativity p_x in
    match placement, assoc with
    | `L, `R2L -> format_parenthesize nx
    | `R, `L2R -> format_parenthesize nx
    | _, _ -> nx

and format_call f args =
  format_seq [ format_subexpr `L call_precedence f;
	       format_arglist args ]

and format_prefix p ops x ff =
  F.pp_print_string ff ops;
  format_subexpr `R p x ff

and format_postfix p x ops ff =
  format_subexpr `L p x ff;
  F.pp_print_string ff ops

and format_infix p x ops y ff =
  format_subexpr `L p x ff;
  F.pp_print_string ff " ";
  F.pp_print_string ff ops;
  F.pp_print_space ff ();
  format_subexpr `R p y ff

and format_op1 p op x =
  match op with
  | Op1Arith `Neg		-> format_prefix  p "-" x
  | Op1Arith `PreInc		-> format_prefix  p "++" x
  | Op1Arith `PreDec		-> format_prefix  p "--" x
  | Op1Arith `PostInc		-> format_postfix p x "++"
  | Op1Arith `PostDec		-> format_postfix p x "--"
  | Op1Bit `Not			-> format_prefix  p "~" x
  | Op1Logic `Not		-> format_prefix  p "!" x
  | Op1Cast t			-> format_prefix  p ("("^type_to_string t^")") x
  | Op1Deref			-> format_prefix  p "*" x
  | Op1SDeref f			-> format_postfix p x ("->"^f)
  | Op1Ref			-> format_prefix  p "&" x
  | Op1SRef f			-> format_postfix p x ("."^f)

and format_op2 p op x y =
  match op with
  | Op2Arith a ->
      let s = match a with 
      | `Add	-> "+"
      | `Sub	-> "-"
      | `Mul	-> "*"
      | `Div	-> "/"
      | `Mod	-> "%"
      in
      format_infix p x s y
  | Op2Comp r ->
      let s = match r with
      | `Eq	-> "=="
      | `NE	-> "!="
      | `Gt	-> ">"
      | `Lt	-> "<"
      | `GE	-> ">="
      | `LE	-> "<="
      in
      format_infix p x s y
  | Op2Logic l ->
      let s = match l with
      | `And	-> "&&"
      | `Or	-> "||"
      in
      format_infix p x s y
  | Op2Bit b ->
      let s = match b with
      | `And	-> "&"
      | `Or	-> "|"
      | `Xor	-> "^"
      | `Shl	-> "<<"
      | `Shr	-> ">>"
      in
      format_infix p x s y
  | Op2Assign ->
      format_infix p x "=" y
  | Op2ArrSubs ->
      format_seq [ format_subexpr `L p x;
		   format_square_bracket (format_expr x) ]
  | Op2Comma ->
      format_list "," [ format_subexpr `L p x;
			format_subexpr `R p y ]

and format_stmt_expr stmts =
  failwith "TODO"

and format_inline_if p pred bt bf =
  failwith "TODO"

and format_initializer xs =
  failwith "TODO"

and format_designated_initializer xs =
  failwith "TODO"
