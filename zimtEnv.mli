module type ARGS =
  sig
    val name : string
    type dv
    type dt
  end

module type S =
  sig
    type env
    type mutenv

    val make : Zimt.ident -> mutenv
  end

module Make (A : ARGS) : S
  with type env		= (A.dv,A.dt) Zimt.environment
  and type mutenv	= (A.dv,A.dt) Zimt.mutable_environment
