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
type 'a const

(* C Language description hoisted into OCaml (with some new
constructs), using type witnesses and GADTs for added compile time
safety. (why write a typechecker when you can use OCaml's?) *)

type 'a type'
and ('a,'b) field'
and _ x =
  | XLit	: 'a lit				-> 'a x
  | XVar	: 'a var				-> 'a x
  | XOp1	: ('a,'b) op1 * 'a x 			-> 'b x
  | XOp2	: ('a,'b,'c) op2 * 'a x * 'b x 		-> 'c x
  | XDeref	: 'a ptr' x				-> 'a x
  | XField	: 'a x * ('a,'b) field'			-> 'b x
  | XArrSubs	: 'a ptr' x * [<int'|uint'] x		-> 'a x
  | XCall	: ('r,'a) fun' x * ('r,'a) args		-> 'r x
  | XStmtExpr	: st list * 'a x			-> 'a x
  | XIIf	: bool x * 'a x * 'a x			-> 'a x
  | XCast	: 'a type' * 'b x			-> 'a x

and st =
  | CLet	: 'a type' * ident * 'a x		-> st
  | CBlock	: st list				-> st
(*| CIf		: int_t x * st * st option		-> st *)
  | CCond	: (int' x * st) list * st option	-> st
  | CFor	: _ x * bool' x * _ x * st		-> st
  | CSwitch	:
      ([< int'|uint'] as 'a) x * ('a lit * st) list * st option -> st

(* Fully typed argument list/tuple *)
and (_,_) args =
  | AVoid	: ('r,'r) args
  | AApply	: 'a x * ('r,'b) args	-> ('r,'a -> 'b) args

(* Fully typed function signature *)
and (_,_) fun' =
  | FVoid	: 'r type' -> ('r,'r) fun'
  | FLambda	: 'a type' * ident * ('r,'b) fun' -> ('r,'a -> 'b) fun'

and 'a lit = 
  | LInt8	: int -> int8' lit
  | LInt16	: int -> int16' lit
  | LInt32	: int32 -> int32' lit
  | LInt64	: int64 -> int64' lit
  | LNatInt	: nativeint -> natint' lit
  | LUInt8	: int -> uint8' lit
  | LUInt16	: int -> uint16' lit
  | LUInt32	: int64 -> uint32' lit
  | LUInt64	: int64 -> uint64' lit
  | LFloat32	: float -> float32' lit
  | LFloat64	: float -> float64' lit
  | LQuoted	: string -> 'a lit
  | LStr	: string -> int8' const ptr' lit

and ident = string
and 'a var = 'a type' * ident
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

module Void : TYPE with type t = void'

module Int8 :
    sig
      include TYPE with type t = int8'
    end
module Int16 :
    sig
      include TYPE with type t = int16'
    end
module Int32 :
    sig
      include TYPE with type t = int32'
    end
module Int64 :
    sig
      include TYPE with type t = int64'
    end
module NatInt :
    sig
      include TYPE with type t = natint'
    end
module UInt8 :
    sig
      include TYPE with type t = uint8'
    end
module UInt16 :
    sig
      include TYPE with type t = uint16'
    end
module UInt32 :
    sig
      include TYPE with type t = uint32'
    end
module UInt64 :
    sig
      include TYPE with type t = uint64'
    end
module Bool :
    sig
      include TYPE with type t = bool'
    end
module Float32 :
    sig
      include TYPE with type t = float32'
    end
module Float64 :
    sig
      include TYPE with type t = float64'
    end

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
