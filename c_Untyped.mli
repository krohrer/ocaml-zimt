type rank = int
type associativity = [`L2R|`R2L]

type x = C_UntypedAST.x
type t = C_UntypedAST.t
type ident = C_UntypedAST.ident
type field = C_UntypedAST.field

module Type :
  sig
  end


module Expr :
  sig
    val precedence : x -> rank
    val associativity : rank ->  associativity

    val call_precedence	: rank
    val comma_precedence : rank
  end

module Stmt :
  sig
  end

module Embedded :
  sig
    val var		: ident -> x
    val call		: ident -> x list -> x
    val apply		: x -> x list -> x

    val ( ~- )		: x -> x		(*	-X	*)
    val inc		: x -> x		(*	++X	*)
    val dec		: x -> x		(*	--X	*)
    val postinc		: x -> x		(*    	X++	*)
    val postdec		: x -> x		(* 	X--	*)
    val not		: x -> x		(*	!X	*)
    val lnot		: x -> x		(*	~X	*)
    val cast		: t -> x -> x		(* 	(T)X	*)
    val ( ^! )		: x -> field -> x	(* 	X->F 	*)
    val ( ^ )		: x -> field -> x	(*	X.F	*)
    val ( ! )		: x -> x		(*	*X 	*)
    val ref		: x -> x		(*	&X 	*)

    val ( := )		: x -> x -> x		(*	X=Y	*)
    val ( +! )		: x -> x -> x		(*	*(X+Y), X[Y]	*)

    val ( + )		: x -> x -> x
    val ( - )		: x -> x -> x
    val ( * )		: x -> x -> x
    val ( / )		: x -> x -> x
    val ( mod )		: x -> x -> x		(*	X%Y	*)

    val ( = )		: x -> x -> x		(*	X==Y	*)
    val ( ==)		: x -> x -> x		(*	X==Y	*)
    val ( <> )		: x -> x -> x		(* 	X!=Y	*)
    val ( != )		: x -> x -> x		(*	X!=Y	*)
    val ( > )		: x -> x -> x
    val ( < )		: x -> x -> x
    val ( >= )		: x -> x -> x
    val ( <= )		: x -> x -> x

    val ( && )		: x -> x -> x
    val ( || )		: x -> x -> x

    val ( land )	: x -> x -> x		(*	X&Y	*)
    val ( lor )		: x -> x -> x		(*	X|Y	*)
    val ( lxor )	: x -> x -> x		(*	X^Y	*)
    val ( lsl )		: x -> x -> x		(*	X<<Y	*)
    val ( lsr )		: x -> x -> x		(*	X>>Y	*)
  end
