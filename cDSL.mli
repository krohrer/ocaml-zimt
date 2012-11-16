(** Monadic DSL for Foreign.Code *)
type 'a t

val bind   : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t

val rtype : entry -> type' t
val argi : entry -> int -> arg t
val sig1 : entry -> (type'*arg) t
val sig2 : entry -> (type'*arg*arg) t
val sig3 : entry -> (type'*arg*arg*arg) t
val sig4 : entry -> (type'*arg*arg*arg*arg) t
val sig5 : entry -> (type'*arg*arg*arg*arg*arg) t
val sig6 : entry -> (type'*arg*arg*arg*arg*arg*arg) t
val sig7 : entry -> (type'*arg*arg*arg*arg*arg*arg*arg) t
val sig8 : entry -> (type'*arg*arg*arg*arg*arg*arg*arg*arg) t
val sig9 : entry -> (type'*arg*arg*arg*arg*arg*arg*arg*arg*arg) t
val sign : entry -> (type'*arg array) t

val stmtf : ('a, unit, string, unit t) format4 -> 'a

val declare : type' -> name -> cexpr -> unit t
val set : name -> cexpr -> unit t

val declaref : type' -> name -> ('a, unit, string, unit t) format4 -> 'a
val setf : name -> ('a, unit, string, unit t) format4 -> 'a

val ret : cexpr -> unit t

val call : name -> cexpr array -> cexpr t
val box : type' -> cexpr -> cexpr t
val unbox : type' -> cexpr -> cexpr t

val box_ret : type' -> cexpr -> unit t
val unbox_arg : arg -> cexpr t

val eval : (entry -> unit t) -> entry -> ccode
