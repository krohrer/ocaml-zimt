type 'a type' = {
    t_name : string;
    t_size : int;
    t_align : int;
  }

(* type witnesses for builtin types *)
type void'	= [`Void]

type int8'	= [`Int8]
type int16'	= [`Int16]
type int32'	= [`Int32]
type int64'	= [`Int64]

type uint8'	= [`Uint8]
type uint16'	= [`Uint16]
type uint32'	= [`Uint32]
type uint64'	= [`Uint64]

type bool'	= [`Bool]

type float16'	= [`Float16]
type float32'	= [`Float32]
type float64'	= [`Float64]

type int' = [ `Int8 | `Int16 | `Int32 | `Int64 ]
type uint' = [ `Uint8 | `Uint16 | `Uint32 | `Uint64 ]
type float' = [ `Float16 | `Float32 | `Float64 ]

type 'a ptr'
type 'a array'
type 'a struct'
type ('a,'b) fun'

type _ x =
| XLit : 'a lit -> 'a x
| XVar : 'a var -> 'a x
| XOp1 : ('a x -> 'b x) lit * 'a x -> 'b x
| XOp2 : ('a x -> 'b x -> 'c x) lit * 'a x * 'b x -> 'c x
| XDeref : 'a ptr x -> 'a x
| XField : 'a struct' * ('a,'b) field -> 'b x
| XArrSubs : 'a ptr x * int' x -> 'a x
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
and ('a,'b) field = ident

exception AlreadyDefined of string

let already_defined_exc fmt = Printf.ksprintf (fun s -> AlreadyDefined s) fmt

module Type :
    sig
      type 'a t = 'a type'

      val make : name:ident -> size:int -> align:int -> 'a t
      val name : 'a t -> ident
      val size : 'a t -> int
      val align : 'a t -> int
    end

module type TYPE =
    sig
      type w
      type t = w type'
	  
      val repr : w type'
      val name : id
      val size : int
      val align : int
    end

module MakeType (sig val name : ident val size : int val align : int end) : TYPE =
  struct
    type w

    let repr = {}
  end

module Int8 :
    sig
      include TYPE with type w = int8'
    end
    =
  struct
    include MakeType (struct let name = "int8_t" let size = 1 let align = 1 end) with type w = int8'
  end
      


module Builtin :
    sig
      val requires : string list

      val int8	      	: int8 type'
      val int16		: int16 type'
      val int32		: int32 type'
      val int64		: int64 type'

      val uint8		: uint8 type'
      val uint16	: uint16 type'
      val uint32	: uint32 type'
      val uint64	: uint64 type'

      val bool		: bool type'

      val float32	: float32 type'
      val float64	: float64 type'
    end
    =
  struct
    (* these values should be dependent on the platform *)
    let requires = [ "stdint.h"; "stdbool.h" ]

    let make name size align = {
      t_name	= name;
      t_size	= size;
      t_align	= align;
    }

    let int8	= make "int8_t" 1 1
    let int16	= make "int16_t" 2 2
    let int32	= make "int32_t" 4 4
    let int64	= make "int64_t" 8 8

    let uint8	= make "uint8_t" 1 1
    let uint16	= make "uint16_t" 2 2
    let uint32	= make "uint32_t" 4 4
    let uint64	= make "uint64_t" 8 8

    let bool	= make "bool" 1 1

    let float32	= make "float" 4 4
    let float64	= make "double" 8 8
  end

module DefStruct (New : sig type t val name : ident end) :
    sig
      type t = New.t struct' type'
      type 'a f = (t,'a) field

      val add_field : 'a type' -> ident -> 'a f
    end
    =
  struct
    type t = New.t struct' type'
    type 'a f = (t,'a) field

    let name = New.name
    let fields = ref []

    let add_field ty id =
      if List.mem_assoc id !fields then
	raise (already_defined_exc "Struct %s: field %s" name id);
      fields := (ty, id) :: !fields;
      id
  end
