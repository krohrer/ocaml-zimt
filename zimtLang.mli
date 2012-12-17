type 'a t = 'a ZimtAST.t
type 'a x = 'a ZimtAST.x
type ('r,'a) fn = ('r,'a) ZimtAST.fn
type ident = ZimtAST.ident

val void : unit t
val int : int t
val bool : bool t
val string : string t
val fn : ('r x,'a) fn -> ('r x,'a) fn t

(* right associative operator to concatenate argument list *)
val (^^) : ('a -> 'b) -> 'a -> 'b
val returns : 'r t -> ('r x,'r x) fn
val param : 'a t -> ident -> ('r x,'b) fn -> ('r x,'a x -> 'b) fn
(* e.g. (param int "a" ^^ param bool "b" ^^ returns unit) *)

val intL : int -> int x
val boolL : bool -> bool x
val stringL : string -> string x

val bind : ('r x,'s) fn -> 's -> 'r x
val mkcall : ('r x,'s) fn -> ('r x,'s) fn x -> 's

(* Imperative modules for easy interface definition *)

type header = string

module type MODULE_DESC =
  sig
    val name : ident
    val headers : header list
  end

module Module (MD : MODULE_DESC) :
  sig
    module Desc : MODULE_DESC

    val extern : ident -> ('r x,'s) fn -> 's
    val defun : ident -> ('r x,'s) fn -> 's -> 's

    val write_ocaml_interface : out_channel -> unit
    val write_ocaml_impl : out_channel -> unit
    val write_c_header : out_channel -> unit
    val write_c_impl : out_channel -> unit
  end
