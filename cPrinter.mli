type pp = PrettyPrinter.t

val pp_type : ?partial:pp -> C.t -> pp
val pp_expr : C.x -> pp
val pp_code : C.code -> pp

val pp_decl : C.declaration -> pp
val pp_defvar : C.defvar -> pp
val pp_defunc : C.defunc -> pp
val pp_typedef : C.typedef -> pp
