type rank = int
type associativity = [`L2R|`R2L]
(* type x = C_UntypedAST.x *)
(* type t = C_UntypedAST.t *)
(* type arg = C_UntypedAST.arg *)
(* type type_qual = C_UntypedAST.type_qual *)
(* type type_sepc = C_UntypedAST.type_spec *)
(* type sign_spec = C_UntypedAST.sign_spec *)
(* type arity = C_UntypedAST.arity *)
(* type int_t = C_UntypedAST.int_t *)

include module type of C_UntypedAST

module Type :
  sig
    val fold_right :
      ?f'void	:(type_qual list		-> 'a -> 'a) ->
      ?f'bool	:(type_qual list		-> 'a -> 'a) ->
      ?f'int	:(type_qual list * int_t	-> 'a -> 'a) ->
      ?f'real	:(type_qual list * real_t	-> 'a -> 'a) ->
      ?f'ref	:(type_qual list * ref_t	-> 'a -> 'a) ->
      ?f'ptr	:(type_qual list * t		-> 'a -> 'a) ->
      ?f'fun	:(type_qual list * fun_t	-> 'a -> 'a) ->
      ?f'arr	:(type_qual list * arr_t	-> 'a -> 'a) ->
      t ->  'a -> 'a
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
