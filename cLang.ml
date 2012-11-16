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

(* C Language description hoisted into OCaml (with some new constructs) *)
type 'a type_t = type_desc
and ('a,'b) field_t = field_desc
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
and type_desc = {
    t_name : string;
    t_size : int;
    t_align : int;
  }
and field_desc = {
    f_name : string;
    f_offset : int;
    f_type : type_desc;
  }

exception AlreadyDefined of string

let already_defined_exc fmt = Printf.ksprintf (fun s -> AlreadyDefined s) fmt

module Type :
    sig
      type 'a t = 'a type_t

      val make : name:ident -> size:int -> align:int -> 'a t
      val name : 'a t -> ident
      val size : 'a t -> int
      val align : 'a t -> int
    end
    =
  struct
    type 'a t = 'a type_t

    let make ~name ~size ~align = {
      t_name	= name;
      t_size	= size;
      t_align	= align;
    }

    let name t = t.t_name
    let size t = t.t_size
    let align t  = t.t_align
  end

module type TYPE =
    sig
      type t
      type r = t Type.t

      val requires : string list
	  
      val repr : r
    end

module Int8 : TYPE with type t = int8' =
  struct
    type t = int8'
    type r = t Type.t

    let requires = [ "stdint.h" ]

    let repr = Type.make ~name:"int8_t" ~size:1 ~align:1
  end
      
module Int16 : TYPE with type t = int16' =
  struct
    type t = int16'
    type r = t Type.t

    let requires = [ "stdint.h" ]

    let repr = Type.make ~name:"int16_t" ~size:2 ~align:2
  end

module Int32 : TYPE with type t = int32' =
  struct
    type t = int32'
    type r = t Type.t

    let requires = [ "stdint.h" ]

    let repr = Type.make ~name:"int32_t" ~size:4 ~align:4
  end

module Int64 : TYPE with type t = int64' =
  struct
    type t = int64'
    type r = t Type.t

    let requires = [ "stdint.h" ]

    let repr = Type.make ~name:"int64_t" ~size:8 ~align:8
  end

module UInt8 : TYPE with type t = uint8' =
  struct
    type t = uint8'
    type r = t Type.t

    let requires = [ "stdint.h" ]

    let repr = Type.make ~name:"uint8_t" ~size:1 ~align:1
  end
      
module UInt16 : TYPE with type t = uint16' =
  struct
    type t = uint16'
    type r = t Type.t

    let requires = [ "stdint.h" ]

    let repr = Type.make ~name:"uint32_t" ~size:2 ~align:2
  end

module UInt32 : TYPE with type t = uint32' =
  struct
    type t = uint32'
    type r = t Type.t

    let requires = [ "stdint.h" ]

    let repr = Type.make ~name:"uint32_t" ~size:4 ~align:4
  end

module UInt64 : TYPE with type t = uint64' =
  struct
    type t = uint64'
    type r = t Type.t

    let requires = [ "stdint.h" ]

    let repr = Type.make ~name:"uint64_t" ~size:8 ~align:8
  end

module Bool : TYPE with type t = bool' =
  struct
    type t = bool'
    type r = t Type.t
	  
    let requires = [ "stdbool.h" ]

    let repr = Type.make ~name:"bool" ~size:1 ~align:1
  end

module Float32 : TYPE with type t = float32' =
  struct
    type t = float32'
    type r = t Type.t

    let requires = []

    let repr = Type.make ~name:"float" ~size:4 ~align:4
  end

module Float64 : TYPE with type t = float64' =
  struct
    type t = float64'
    type r = t Type.t

    let requires = []

    let repr = Type.make ~name:"double" ~size:8 ~align:8
  end

module Struct (NewT : sig type t val name : ident end) :
    sig
      type t = NewT.t struct'
      type r = t Type.t
      type 'b f = (NewT.t,'b) field_t

      val add_field : 'b Type.t -> ident -> 'b f

      val struct_repr : unit -> r
    end
    =
  struct
    type t = NewT.t struct'
    type r = t Type.t
    type 'b f = (NewT.t,'b) field_t

    let name = NewT.name
    let fields = ref []
    let size = ref 0
    let max_align = ref 0

    let align_size align =
      (!size + align - 1) land (align - 1)

    let add_field ft id =
      if List.exists (fun d -> d.f_name = id) !fields then
	raise (already_defined_exc "Struct %s: field %s" name id)
      else begin
  	let align = Type.align ft in
	let o = align_size (Type.align ft) in
	let field =
	  { f_name = id;
	    f_type = ft;
	    f_offset = o }
	in
	fields := field :: !fields;
	size := o + align;
	max_align := max !max_align align;
	field
      end
	  
    let struct_repr () = 
      let align = !max_align in
      let size = align_size align in
      Type.make ~name ~size ~align
  end
