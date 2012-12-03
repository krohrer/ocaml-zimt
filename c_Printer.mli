type pp

(* Pretty printers as a monoid structure *)
val (>>>) : pp -> pp -> pp
val pp_empty : pp

val pp_string : string -> pp
val pp_to_string : ('a -> string) -> 'a -> pp
val pp_format : ('a, unit, string, pp) format4 -> 'a
val pp_space : pp
val pp_prefix : string -> pp
val pp_suffix : string -> pp
val pp_list : elem:('a -> pp) -> ?sep:pp -> 'a list -> pp
val pp_seq : pp list -> pp
val pp_bracket : string -> pp -> string -> pp
val pp_parenthesize : pp -> pp
val pp_bracket_curly : pp -> pp
val pp_bracket_square : pp -> pp

val pp_type : ?partial:pp -> C_Untyped.t -> pp
