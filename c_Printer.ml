open C_Untyped

type pp = Format.formatter -> unit

(* Pretty printers form a monoid *)
let pp_empty ff = ()
let (>>>) f g = fun ff -> f ff; g ff

(* f >>> pp_emtpy === pp_empty >>> f *)
(* (f >>> g) >>> h === f >>> (g >>> h) *)

let pp_string s ff = Format.pp_print_string ff s
let pp_int ff i = Format.pp_print_int ff i
let pp_format fmt = Format.ksprintf (fun s ff -> Format.pp_print_string ff s) fmt
let pp_space ff = Format.pp_print_space ff ()

let pp_to_string f x ff = Format.pp_print_string ff (f x)
let pp_nbsp ff = Format.pp_print_string ff " "

let pp_prefix s = pp_string s >>> pp_space
let pp_suffix s = pp_space >>> pp_string s

let pp_list ~ppelem ~ppsep list ff =
  let rec fold = function
    | []	-> ()
    | [x]	-> ppelem x ff
    | x::rest	-> ppelem x ff; ppsep ff; fold rest
  in
  fold list

let pp_seq = List.fold_left (>>>) pp_empty
  
let pp_bracket sopen pp sclose = pp_string sopen >>> pp >>> pp_string sclose
let pp_parenthesize pp = pp_bracket "(" pp ")"
let pp_bracket_curly pp = pp_bracket "{" pp "}"
let pp_bracket_square pp = pp_bracket "[" pp "]"

let pp_comma = pp_string "," >>> pp_space
    
(* Types *)

let pp_sign_spec = function
  | `unsigned	-> pp_prefix "unsigned"
  | `signed	-> pp_prefix "signed"
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

let pp_prim = function
  | TVoid		-> pp_string "void"
  | TBool		-> pp_string "bool"
  | TInt (ss,it)	-> pp_sign_spec ss >>> pp_to_string int_type_to_string it
  | TReal rt		-> pp_to_string real_type_to_string rt

let type_qual_to_string = function
  | `const	-> "const"
  | `restrict	-> "restrict"
  | `volatile	-> "volatile"

let pp_type_quals qs =
  pp_list
    ~ppelem:(pp_to_string type_qual_to_string)
    ~ppsep:pp_nbsp
    qs

let pp_type t =
  pp_empty


(*
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
  | XOp1 (op,a)			-> format_op1 (Expr.precedence x) op a
  | XOp2 (op,a,b)		-> format_op2 (Expr.precedence x) op a b
  | XStmtExpr stmts		-> format_stmt_expr stmts
  | XIIf (p,t,f)		-> format_inline_if (Expr.precedence x) p t f
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
  let p_x = Expr.precedence x in
  let x' = format_expr x in
  if p_x < Expr.comma_precedence then
    x'
  else
    format_parenthesize x'

and format_arglist args =
  let ns = List.map format_comma_separated_element args in
  format_parenthesize (format_list "," ns)

and format_subexpr placement p_parent x =
  let p_x = Expr.precedence x in
  let nx = format_expr x in
  if p_x < p_parent then
    nx
  else if p_x > p_parent then
    format_parenthesize nx
  else
    let assoc = Expr.associativity p_x in
    match placement, assoc with
    | `L, `R2L -> format_parenthesize nx
    | `R, `L2R -> format_parenthesize nx
    | _, _ -> nx

and format_call f args =
  format_seq [ format_subexpr `L Expr.call_precedence f;
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
  | Op1StructDeref f		-> format_postfix p x ("->"^f)
  | Op1Ref			-> format_prefix  p "&" x
  | Op1StructRef f		-> format_postfix p x ("."^f)

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
      | `ShiftL	-> "<<"
      | `ShiftR	-> ">>"
      in
      format_infix p x s y
  | Op2Assign ->
      format_infix p x "=" y
  | Op2Subscript ->
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
*)
