open ZimtAST

module type ARGS =
  sig
    val name		: ident
    val env		: mutenv
  end

module Make (A : ARGS) : STRUCT =
  struct
    type s = unit
    type w = s struct'

    let repr : 'a struct' ref = ref (SZero ())

    let t' = TForward (lazy (TStruct !repr))
    let name' = A.name

    let field' : 'a t -> ident -> (w,'a) field = fun ft fn ->
      let f = FNamed (t', ft, fn) in
      A.env#add_type fn (Type ft);
      repr := SPlusField (!repr, (ft, fn));
      f
	
    let bits' : int t -> ident -> int -> (w,'a) field = fun ft fn bc ->
      let f = FNamed (t', ft, fn) in
      A.env#add_type fn (Type ft);
      repr := SPlusBits (!repr, (ft, fn, bc));
      f

    let pad' : int t -> int -> unit = fun ft bc ->
      repr := SPlusPadding (!repr, (ft, bc))
  end

let make env name = (module Make (struct
	  let name = name
	  let env = env
end) : STRUCT)

