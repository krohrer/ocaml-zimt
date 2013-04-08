open Zimt

module Make (A : sig
  val name	: ident
  val env	: mutenv
  val requires	: header list
end) : ENUM =
  struct
    type e = unit
    type w = e enum

    let env' = A.env#env
    let requires' = []

    let repr : 'a enum ref = ref (EZero ())

    let t' = TForward (lazy (TEnum !repr))
    let name' = A.name

    let case' n l =
      A.env#add_value n (DefVar (XLit l));
      repr := EPlus (!repr, (n,l));
      XId (t', (A.env#env,n))
  end

let make env name hs = (module Make (struct
  let name = name
  let env = env
  let requires = hs
end) : ENUM)
