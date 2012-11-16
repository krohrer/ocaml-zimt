(* Type witnesses for builtin types *)
type void'	= [`Void]

type int8'	= [`Int8]
type int16'	= [`Int16]
type int32'	= [`Int32]
type int64'	= [`Int64]

type natint'    = [`NatInt]

type uint8'	= [`UInt8]
type uint16'	= [`UInt16]
type uint32'	= [`UInt32]
type uint64'	= [`UInt64]

type bool'	= [`Bool]

type float16'	= [`Float16]
type float32'	= [`Float32]
type float64'	= [`Float64]

type int' = [ `Int8 | `Int16 | `Int32 | `Int64 | `NatInt ]
type uint' = [ `UInt8 | `UInt16 | `UInt32 | `UInt64 ]
type float' = [ `Float16 | `Float32 | `Float64 ]

type 'a ptr'
type 'a array'
type 'a struct'
type ('a,'b) fun'

(* C Language description hoisted into OCaml (with some new constructs) *)
type 'a type'
and ('a,'b) field'
and _ x =
| XLit : 'a lit -> 'a x
| XVar : 'a var -> 'a x
| XOp1 : ('a x -> 'b x) lit * 'a x -> 'b x
| XOp2 : ('a x -> 'b x -> 'c x) lit * 'a x * 'b x -> 'c x
| XDeref : 'a ptr' x -> 'a x
| XField : 'a struct' * ('a,'b) field' -> 'b x
| XArrSubs : 'a ptr' x * int' x -> 'a x
| XCall : ('a -> 'b x) x * 'a -> 'b x
| XStmtExpr : st list * 'a x -> 'a x
| XIIf : bool x * 'a x * 'a x -> 'a x

and st =
| CDecl : 'a type' * ident * 'a x option -> st
| CComp : st list -> st
(*| CIf : int_t x * st * st option -> st *)
| CCond : (int' x * st) list * st option -> st
| CFor : _ x * bool' x * _ x * st -> st
| CSwitch : 'a x * ('a lit * st) list * st option -> st

and ident = string
and 'a var = 'a type' * ident
and 'a lit = string
and type_repr
and field_repr
and header = [ `Sys of string | `Usr of string ]

(* C language types *)
exception AlreadyDefined of string

module type TYPE =
  sig
    type w (* Type witness / phantom type *)
    type t = w type' (* OCaml type encoding of C type *)

    val t : t (* strongly typed representation of C type *)
    val r : type_repr (* untyped representation of C type *)
  end

module Int8	: TYPE with type w = int8'
module Int16	: TYPE with type w = int16'
module Int32	: TYPE with type w = int32'
module Int64	: TYPE with type w = int64'
module NatInt	: TYPE with type w = natint'
module UInt8	: TYPE with type w = uint8'
module UInt16	: TYPE with type w = uint16'
module UInt32	: TYPE with type w = uint32'
module UInt64	: TYPE with type w = uint64'
module Bool	: TYPE with type w = bool'
module Float32	: TYPE with type w = float32'
module Float64	: TYPE with type w = float64'

(* Untyped representation of C type *)
module TypeRepr :
    sig
      type t = type_repr

      val name		: t -> ident
      val requires	: t -> header list
    end

(*
module Struct (NewT : sig type t val name : ident val requires : string list end) :
    sig
      type t = NewT.t struct'
      type r = t Type.t
      type 'b f = (NewT.t,'b) field'

      val add_field : 'b Type.t -> ident -> 'b f

      val struct_repr : unit -> r
    end
 *)
