type rank = int
type associativity = [`L2R|`R2L]

val precedence : C.x -> rank
val associativity : rank ->  associativity

val call_precedence	: rank
val comma_precedence : rank
val assign_precedence : rank
