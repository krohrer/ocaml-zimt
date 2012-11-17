(* Type witnesses for builtin types *)

(* void' is not a polymorphic variant on purpose, so it cannot be unified with
scalar types *)
type void' = unit

(* Scalar types *)
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
(* Builtin type families *)
type int' = [ `Int8 | `Int16 | `Int32 | `Int64 | `NatInt ]
type uint' = [ `UInt8 | `UInt16 | `UInt32 | `UInt64 ]
type float' = [ `Float16 | `Float32 | `Float64 ]
(* Builtin composite types *)
type 'a ptr'
type 'a array'
type 'a struct'
type ('a,'b) fun' (* Iff the function has multiple arguments, 'a is a n-tuple *)

(* C Language description hoisted into OCaml (with some new
constructs), using type witnesses and GADTs for added compile time
safety. (why write a typechecker when you can use OCaml's?) *)

type 'a type'
and ('a,'b) field'
and _ x =
| XLiteral	: 'a literal				-> 'a x
| XUnsafeFun	: ('a*'b) literal			-> ('a,'b) fun' x
| XVar		: 'a var				-> 'a x
| XOp1		: ('a,'b) op1 * 'a x 			-> 'b x
| XOp2		: ('a,'b,'c) op2 * 'a x * 'b x 		-> 'c x
| XDeref	: 'a ptr' x				-> 'a x
| XField	: 'a x * ('a,'b) field'			-> 'b x
| XArrSubs	: 'a ptr' x * int' x			-> 'a x
| XCall		: ('a,'b) fun' x * 'a			-> 'b x
| XStmtExpr	: st list * 'a x			-> 'a x
| XIIf		: bool x * 'a x * 'a x			-> 'a x

and st =
| CLet		: 'a type' * ident * 'a x option		-> st
| CComp		: st list					-> st
(*| CIf		: int_t x * st * st option			-> st *)
| CCond		: (int' x * st) list * st option		-> st
| CFor		: _ x * bool' x * _ x * st			-> st
| CSwitch	: 'a x * ('a literal * st) list * st option	-> st

and ident = string
and 'a var = 'a type' * ident
and 'a literal = string
and ('a,'b) op1 = 'a -> 'b
and ('a,'b,'c) op2 = 'a -> 'b -> 'c
and type_repr
and field_repr
and header = [ `Sys of string | `Usr of string ]

(* C language types *)

exception AlreadyDefined of string

(* Untyped representation of C type *)
module TypeRepr :
    sig
      type t = type_repr

      val name		: t -> ident
      val defined	: t -> bool
      val requires	: t -> header list

      val of_typed : 'a type' -> t
    end

(* Typed representation of C type *)
module type TYPE =
  sig
    type t (* Type witness / phantom type *)

    val t : t type' (* strongly typed representation of C type *)
  end

(* Functor argument *)
module type TYPE_DESC =
    sig
      type t
      val name : ident
      val defined : bool
      val requires : header list
    end

module Void	: TYPE with type t = void'
module Int8	: TYPE with type t = int8'
module Int16	: TYPE with type t = int16'
module Int32	: TYPE with type t = int32'
module Int64	: TYPE with type t = int64'
module NatInt	: TYPE with type t = natint'
module UInt8	: TYPE with type t = uint8'
module UInt16	: TYPE with type t = uint16'
module UInt32	: TYPE with type t = uint32'
module UInt64	: TYPE with type t = uint64'
module Bool	: TYPE with type t = bool'
module Float32	: TYPE with type t = float32'
module Float64	: TYPE with type t = float64'

(* Facilitate definition of structs *)
module StructMixin (D : TYPE_DESC) :
    sig
      val add_field : 'a type' -> ident -> (D.t struct','a) field'
	  
      val make_type : unit -> D.t struct' type'
    end

module CustomStruct :
    sig
      type s
      include TYPE with type t = s struct'
      type 'a f = (t,'a) field'

      val some_field : Int8.t f
      val other_field : Bool.t f
    end

module AnotherStruct :
    sig
      type s
      include TYPE with type t = s struct'
      type 'a f = (t,'a) field'

      val some_field : Int8.t f
      val other_field : Bool.t f
    end
