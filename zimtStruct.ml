open Zimt

module Make (A : sig
  val name	: ident
  val env	: mutenv
  val requires	: header list
end) : STRUCT =
  struct
    type s = unit
    type w = s struct'

    let env' = A.env#env
    let requires' = A.requires

    let repr : 'a struct' ref = ref (SZero ())

    let t' = TForward (lazy (TStruct !repr))
    let name' = A.name

    let field' : 'a t -> ident -> (w,'a) field = fun ft fn ->
      let f = FNamed (t', ft, fn) in
      repr := SPlusField (!repr, (ft, fn));
      f
	
    let bits' : int t -> ident -> int -> (w,'a) field = fun ft fn bc ->
      let f = FNamed (t', ft, fn) in
      repr := SPlusBits (!repr, (ft, fn, bc));
      f

    let pad' : int t -> int -> unit = fun ft bc ->
      repr := SPlusPadding (!repr, (ft, bc))
  end

let make env name hs = (module Make (struct
	  let name = name
	  let env = env
	  let requires = hs
end) : STRUCT)

