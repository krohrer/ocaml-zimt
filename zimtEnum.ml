open Zimt

module type ARGS =
  sig
    val name	: ident
    val env	: mutenv
  end

module Make (A : ARGS) : ENUM =
  struct
    type e = unit
    type w = e enum

    let repr : 'a enum ref = ref (EZero ())

    let type' = TForward (lazy (TEnum !repr))
    let name' = A.name

    let case' n l =
      A.env#add_value n (DefVar (XLit l));
      repr := EPlus (!repr, (n,l));
      XId (type', (A.env#env,n))
  end

let make env name = (module Make (struct
  let name = name
  let env = env
end) : ENUM)
