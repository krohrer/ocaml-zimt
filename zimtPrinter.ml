open PrettyPrinter
type pp = PrettyPrinter.t

open Zimt

let rec dump_t : type t . t Zimt.t -> pp = function
  | TCaml ml		-> dump_caml_t ml
  | TForward (lazy t')	-> dump_forward t'
  | TNamed (t',id)	-> dump_named t' id
  | TPtr p		-> dump_ptr p
  | TStruct s		-> dump_struct' s
  | TEnum e		-> dump_enum e
  | TPrim prim		-> dump_prim prim

and dump_caml_t : type t . t Caml.t -> pp = fun ml ->
  pp_string "TODO"

and dump_forward : type t . t Zimt.t -> pp = fun t' ->
  pp_string "TODO"

and dump_named : type t . t Zimt.t -> q_ident -> pp = fun t' id ->
  pp_string "TODO"

and dump_ptr : type t . t Zimt.ptr -> pp = function
  | _ -> pp_string "TODO"

and dump_struct' : type t . t Zimt.struct' -> pp =
  function
  | _ -> pp_string "TODO"

and dump_enum : type t . t Zimt.enum -> pp =
  function
  | _ -> pp_string "TODO"

and dump_prim : type t . t Zimt.prim -> pp =
  function
  | _ -> pp_string "TODO"


(* and dump_forward : type t . t Zimt.t Lazy.t = fun (lazy t') -> *)
(*   pp_string "" *)

let dump_type = function
  | _ -> failwith "TODO"
