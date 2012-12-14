type x = C_UntypedAST.x
type t = C_UntypedAST.t
type s = C_UntypedAST.s
type lit = C_UntypedAST.lit
type ident = C_UntypedAST.ident
type field = C_UntypedAST.field

(* Basic types *)

(* Expressions *)
val var		: ident -> x
val call	: ident -> x list -> x
val apply	: x -> x list -> x

val ( ~- )	: x -> x		(*	-X	*)
val inc		: x -> x		(*	++X	*)
val dec		: x -> x		(*	--X	*)
val postinc	: x -> x		(*    	X++	*)
val postdec	: x -> x		(* 	X--	*)
val not		: x -> x		(*	!X	*)
val lnot	: x -> x		(*	~X	*)
val cast	: t -> x -> x		(* 	(T)X	*)
val sderef	: x -> field -> x	(*	X->F	*)
val sref	: x -> field -> x	(*	X.F	*)
val ( ! )	: x -> x		(*	*X 	*)
val ref		: x -> x		(*	&X 	*)

val ( := )	: x -> x -> x		(*	X=Y		*)
val idx		: x -> x -> x		(*	*(X+Y), X[Y]	*)

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

(* Literals *)
val intlit	: int -> x
val strlit	: string -> x

(* Statements *)
val block	: s list -> s

val expr	: x -> s
val decl	: t -> ident -> x -> s

val switch	: x -> s list -> s
val case	: ident -> s list -> s
val lcase	: lit -> s list -> s
val default	: s list -> s

val for_ever	: s list -> s
val for'	: t -> ident -> x -> x -> x -> s list -> s
val while'	: x -> s list -> s
val do_while	: s list -> x -> s
val if'		: x -> s list -> s list -> s

val return	: x -> s
val break 	: s
val continue 	: s
val goto	: ident -> s
val label	: ident -> s
