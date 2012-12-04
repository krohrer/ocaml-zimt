type pp

(* Pretty printers as a monoid structure, or whatever you wanna call it. *)
val (>>>) : pp -> pp -> pp
val pp_empty : pp

val pp_string : string -> pp
val pp_to_string : ('a -> string) -> 'a -> pp
val pp_format : ('a, unit, string, pp) format4 -> 'a
val pp_space : pp
val pp_list : elem:('a -> pp) -> ?sep:pp -> 'a list -> pp
val pp_seq : pp list -> pp
val pp_bracket : string -> pp -> string -> pp
val pp_parenthesize : pp -> pp
val pp_bracket_curly : pp -> pp
val pp_bracket_square : pp -> pp

val pp_box : ?indent:int -> pp -> pp
val pp_vbox : ?indent:int -> pp -> pp
val pp_hbox : pp -> pp
val pp_hvbox : ?indent:int -> pp -> pp

(* The interesting stuff *)

val pp_type : ?partial:pp -> C_UntypedAST.t -> pp
val pp_expr : C_UntypedAST.x -> pp 

val run_on_stdout : pp -> unit
