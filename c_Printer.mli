type pp = Format.formatter -> unit

(* Pretty printers as a monoid structure, or whatever you wanna call it. *)
val (+++) : pp -> pp -> pp
val pp_empty : pp

val ( *** ) : (pp -> pp) -> pp -> pp

val pp_string : string -> pp
val pp_to_string : ('a -> string) -> 'a -> pp
val pp_format : ('a, unit, string, pp) format4 -> 'a
val pp_space : pp
val pp_nbsp : pp
val pp_cut : pp
val pp_break : int -> int -> pp
val pp_list : elem:('a -> pp) -> ?sep:pp -> 'a list -> pp
val pp_seq : pp list -> pp
val pp_bracket : string -> string -> pp -> pp
val pp_parenthesize : pp -> pp
val pp_bracket_curly : pp -> pp
val pp_bracket_square : pp -> pp

val pp_box : ind:int -> pp -> pp
val pp_vbox : ind:int -> pp -> pp
val pp_hbox : pp -> pp
val pp_hvbox : ind:int -> pp -> pp

(* The interesting stuff *)

val pp_type : ?partial:pp -> C_UntypedAST.t -> pp
val pp_decl : C_UntypedAST.t -> C_UntypedAST.ident -> C_UntypedAST.x option -> pp
val pp_expr : C_UntypedAST.x -> pp 
val pp_stmt : C_UntypedAST.st -> pp
