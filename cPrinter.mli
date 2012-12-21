val pp_comma : PrettyPrinter.t

val pp_type : ?partial:PrettyPrinter.t -> C.t -> PrettyPrinter.t
val pp_decl : C.t -> C.ident -> C.x option -> PrettyPrinter.t
val pp_expr : C.x -> PrettyPrinter.t
val pp_code : C.code -> PrettyPrinter.t
