module type ARGS =
  sig
    val name : Zimt.ident
    val includes : Zimt.header list
  end

module Make (A : ARGS) : Zimt.MODULE

val module' : Zimt.ident -> Zimt.header list -> (module Zimt.MODULE)
