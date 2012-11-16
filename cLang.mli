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
| XField : 'a * ('a,'b) field' -> 'b x
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

(* Untyped representation of C type *)
module TypeRepr :
    sig
      type t = type_repr

      val name		: t -> ident
      val defined	: t -> bool
      val requires	: t -> header list
    end

module type TYPE =
  sig
    type t (* Type witness / phantom type *)

    val t : t type' (* strongly typed representation of C type *)
    val r : type_repr (* untyped representation of C type *)
  end

module type TYPE_DESC =
    sig
      type t
      val name : ident
      val defined : bool
      val requires : header list
    end

module ScalarType (D : TYPE_DESC) : TYPE with type t = D.t

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

module StructMixin (D : TYPE_DESC) :
    sig
      val add_field : 'a type' -> ident -> (D.t struct','a) field'
	  
      val make_type : unit -> D.t struct' type'
      val make_repr : unit -> TypeRepr.t
    end

module CustomStruct :
    sig
      type s
      type 'a f = (s,'a) field'
      include TYPE with type t = s struct'

      val some_field : Int8.t f
      val other_field : Bool.t f
    end
