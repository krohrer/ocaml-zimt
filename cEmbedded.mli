type x = C.x
type t = C.t
type code = C.code
type lit = C.lit
type ident = C.ident
type field = C.field

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
val block	: code list -> code

val expr	: x -> code
val decl	: t -> ident -> x -> code

val switch	: x -> code list -> code
val case	: ident -> code list -> code
val lcase	: lit -> code list -> code
val default	: code list -> code

val for_ever	: code list -> code
val for'	: t -> ident -> x -> x -> x -> code list -> code
val while'	: x -> code list -> code
val do_while	: code list -> x -> code
val if'		: x -> code list -> code list -> code

val return	: x -> code
val break 	: code
val continue 	: code
val goto	: ident -> code
val label	: ident -> code
