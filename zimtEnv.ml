open ZimtAST

(* Exceptional conditions *)
module Conditions =
  struct
    let already_defined mn n =
      failwith (Printf.sprintf "%s: Already defined: %s" mn n)
  end

(* Environments *)
module HeaderSet = Set.Make(struct
  type t = ZimtAST.header
  let compare h1 h2 = compare h1 h2
end)

module EnvSet = Set.Make(struct
  type t = env
  let compare e1 e2 = compare e1 e2 (* compare (Oo.id e1) (Oo.id e2) *)
end)

let make name =
object(self : mutenv)
  val types = Hashtbl.create 128
  val values = Hashtbl.create 128
  val mutable includes = HeaderSet.empty
  val mutable requires = EnvSet.empty

  method env = (self :> env)

  method includes : header list = HeaderSet.elements includes

  method requires : env list = EnvSet.elements requires

  method add_include h =
    includes <- HeaderSet.add h includes

  method lookup_value n =
    try Some (Hashtbl.find values n) with Not_found -> None

  method lookup_type n =
    try Some (Hashtbl.find types n) with Not_found -> None

  method add_value n v =
    match self#lookup_value n with
    | None ->
	      Hashtbl.add values n v
	      (* failwith "TODO: Add requires from values" *)
    | Some _ ->
	      Conditions.already_defined name n

  method add_type n t =
    match self#lookup_type n with
    | None ->
	      Hashtbl.add types n t
	      (* failwith "TODO: Add requires from values" *)
    | Some _ ->
	      Conditions.already_defined name n
end
