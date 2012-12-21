type t = Format.formatter -> unit

(* Pretty printers as a monoid structure, or whatever you wanna call it.

    f +++ pp_emtpy === pp_empty +++ f
    (f +++ g) +++ h === f +++ (g +++ h)
*)
val (+++) : t -> t -> t
val pp_empty : t


val ( *** ) : (t -> t) -> t -> t

val pp_string		: string -> t
val pp_int		: int -> t
val pp_to_string	: ('a -> string) -> 'a -> t
val pp_format		: ('a, unit, string, t) format4 -> 'a
val pp_spc		: t
val pp_nbsp		: t
val pp_cut		: t
val pp_brk		: int -> int -> t
val pp_list		: elem:('a -> t) -> ?sep:t -> 'a list -> t
val pp_seq		: t list -> t
val pp_bracket		: string -> string -> t -> t
val pp_parenthesize	: t -> t
val pp_bracket_curly	: t -> t
val pp_bracket_square	: t -> t

val pp_box		: ind:int -> t -> t
val pp_vbox		: ind:int -> t -> t
val pp_hbox		: t -> t
val pp_hvbox		: ind:int -> t -> t
