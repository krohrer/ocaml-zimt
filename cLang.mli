(* Type witnesses for builtin types *)
type void'	= [`Void]

type int8'	= [`Int8]
type int16'	= [`Int16]
type int32'	= [`Int32]
type int64'	= [`Int64]

type uint8'	= [`UInt8]
type uint16'	= [`UInt16]
type uint32'	= [`UInt32]
type uint64'	= [`UInt64]

type bool'	= [`Bool]

type float16'	= [`Float16]
type float32'	= [`Float32]
type float64'	= [`Float64]

type int' = [ `Int8 | `Int16 | `Int32 | `Int64 ]
type uint' = [ `UInt8 | `UInt16 | `UInt32 | `UInt64 ]
type float' = [ `Float16 | `Float32 | `Float64 ]

type 'a ptr'
type 'a array'
type 'a struct'
type ('a,'b) fun'

(* C language description hoisted into OCaml (with some new constructs) *)
type 'a type_t
and ('a,'b) field_t
and _ x =
| XLit : 'a lit -> 'a x
| XVar : 'a var -> 'a x
| XOp1 : ('a x -> 'b x) lit * 'a x -> 'b x
| XOp2 : ('a x -> 'b x -> 'c x) lit * 'a x * 'b x -> 'c x
| XDeref : 'a ptr' x -> 'a x
| XField : 'a struct' * ('a,'b) field_t -> 'b x
| XArrSubs : 'a ptr' x * int' x -> 'a x
| XCall : ('a -> 'b x) x * 'a -> 'b x
| XStmtExpr : st list * 'a x -> 'a x
| XIIf : bool x * 'a x * 'a x -> 'a x

and st =
| CDecl : 'a type_t * ident * 'a x option -> st
| CComp : st list -> st
(*| CIf : int_t x * st * st option -> st *)
| CCond : (int' x * st) list * st option -> st
| CFor : _ x * bool' x * _ x * st -> st
| CSwitch : 'a x * ('a lit * st) list * st option -> st

and ident = string
and 'a var = 'a type_t * ident
and 'a lit = string

(* C language types *)
exception AlreadyDefined of string

module type TYPE =
  sig
    type t
    type r = t type_t

    val requires : string list
	
    val repr : r
  end

module Int8 : TYPE with type t = int8'
module Int16 : TYPE with type t = int16'
module Int32 : TYPE with type t = int32'
module Int64 : TYPE with type t = int64'
module UInt8 : TYPE with type t = uint8'
module UInt16 : TYPE with type t = uint16'
module UInt32 : TYPE with type t = uint32'
module UInt64 : TYPE with type t = uint64'
module Bool : TYPE with type t = bool'
module Float32 : TYPE with type t = float32'
module Float64 : TYPE with type t = float64'

module Type :
    sig
      type 'a t = 'a type_t

      val make : name:ident -> size:int -> align:int -> 'a t
      val name : 'a t -> ident
      val size : 'a t -> int
      val align : 'a t -> int
    end

module Struct (NewT : sig type t val name : ident end) :
    sig
      type t = NewT.t struct'
      type r = t Type.t
      type 'b f = (NewT.t,'b) field_t

      val add_field : 'b Type.t -> ident -> 'b f

      val struct_repr : unit -> r
    end
