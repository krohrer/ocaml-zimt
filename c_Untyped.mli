type rank = int
type associativity = [`L2R|`R2L]

module Type :
sig
  type x = C_UntypedAST.x
  type t = C_UntypedAST.t
  type q = C_UntypedAST.type_qual
  type s = C_UntypedAST.type_spec

  val precedence : s -> rank
  val associativity : rank -> associativity
end

module Expr :
sig
  type t = C_UntypedAST.x

  val precedence : t -> rank
  val associativity : rank ->  associativity

  val call_precedence	: rank
  val comma_precedence	: rank
end

module Stmt :
sig
  type t = C_UntypedAST.st
end

module Ops :
sig
  type t = C_UntypedAST.t
  type x = C_UntypedAST.x
  type field = C_UntypedAST.field
  type ident = C_UntypedAST.ident

  val var	: ident -> x
  val call	: ident -> x list -> x
  val apply	: x -> x list -> x

  val ( ~- )	: x -> x		(*	-X	*)
  val inc	: x -> x		(*	++X	*)
  val dec	: x -> x		(*	--X	*)
  val postinc	: x -> x		(*    	X++	*)
  val postdec	: x -> x		(* 	X--	*)
  val not	: x -> x		(*	!X	*)
  val lnot	: x -> x		(*	~X	*)
  val cast	: t -> x -> x		(* 	(T)X	*)
  val ( ^! )	: x -> field -> x	(* 	X->F 	*)
  val ( ^ )	: x -> field -> x	(*	X.F	*)
  val ( ! )	: x -> x		(*	*X 	*)
  val ref	: x -> x		(*	&X 	*)

  val ( := )	: x -> x -> x		(*	X=Y	*)
  val ( +! )	: x -> x -> x		(*	*(X+Y), X[Y]	*)

  val ( + )	: x -> x -> x
  val ( - )	: x -> x -> x
  val ( * )	: x -> x -> x
  val ( / )	: x -> x -> x
  val ( mod )	: x -> x -> x		(*	X%Y	*)

  val ( = )	: x -> x -> x		(*	X==Y	*)
  val ( ==)	: x -> x -> x		(*	X==Y	*)
  val ( <> )	: x -> x -> x		(* 	X!=Y	*)
  val ( != )	: x -> x -> x		(*	X!=Y	*)
  val ( > )	: x -> x -> x
  val ( < )	: x -> x -> x
  val ( >= )	: x -> x -> x
  val ( <= )	: x -> x -> x

  val ( && )	: x -> x -> x
  val ( || )	: x -> x -> x

  val ( land )	: x -> x -> x		(*	X&Y	*)
  val ( lor )	: x -> x -> x		(*	X|Y	*)
  val ( lxor )	: x -> x -> x		(*	X^Y	*)
  val ( lsl )	: x -> x -> x		(*	X<<Y	*)
  val ( lsr )	: x -> x -> x		(*	X>>Y	*)
end
