type formatter = Format.formatter

val print_expression : formatter -> C.x -> unit
val print_statement : formatter -> C.st -> unit
val print_translation_unit : formatter -> C.tlunit -> unit
