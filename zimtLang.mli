type 'a t = 'a ZimtAST.t
type 'a x = 'a ZimtAST.x
type 'a lit = 'a ZimtAST.lit
type 'a ptr = 'a ZimtAST.ptr
type 'a struct' = 'a ZimtAST.struct'
type 'a enum = 'a ZimtAST.enum
type ('a,'b) field = ('a,'b) ZimtAST.field
type ('r,'a) fn = ('r,'a) ZimtAST.fn
type ident = ZimtAST.ident
type header = string

type global =
  | GlobalConst		: 'a lit		-> global
  | GlobalDefun		: ('r x,'s) fn * 'r x	-> global
  | GlobalExtern	: ('r,'s) fn		-> global

type type' =
  | Type	: 'a t				-> type'

module Fn :
  sig
    (** EDSL for function signatures

	e.g. (arg int "a" ^^ arg bool "b" ^^ arg unit) *)

    (* right associative operator to concatenate argument list *)
    val (^^) : ('a -> 'b) -> 'a -> 'b
    val ret : 'r t -> ('r x,'r x) fn
    val arg : 'a t -> ident -> ('r x,'b) fn -> ('r x,'a x -> 'b) fn

    (** Helpers *)
    val bind : ('r x,'s) fn -> 's -> 'r x
    val mkcall : ('r x,'s) fn -> ('r x,'s) fn x -> 's
  end

module type NAMED =
  sig
    val name' : string
  end

(* CONCRETE TYPE *)
module type TYPE =
  sig
    include NAMED

    type w
    val t' : w t
  end

(* CONCRETE ENUM *)
module type ENUM =
  sig
    type e
    include TYPE with type w = e enum

    val case'	: ident -> int lit -> w x
  end

(* CONCRETE STRUCT *)
module type STRUCT =
  sig
    type s
    include TYPE with type w = s struct'

    val field'	: 'a t -> ident -> (w,'a) field
    val bits'	: int t -> ident -> int -> (w,int) field
    val pad'	: int t -> int -> unit
  end

(* MODULE TYPE *)
module type MODULE =
  sig
    include NAMED

    val enum : ident -> (module ENUM)
    val struct' : ident -> (module STRUCT)

    val defun : ident -> ('r x,'s) fn -> 's -> 's
    val extern : ident -> ('r x,'s) fn -> 's
  end

(* MODULE DESCRIPTION *)
module type MODULE_DESC =
  sig
    include NAMED

    type module' = (module MODULE)
    val requires : module' list
  end

(* DEFINE NEW MODULE *)
module DefModule (D : MODULE_DESC) : MODULE

