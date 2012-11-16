(* Type witnesses for builtin types *)
type void'	= [`Void]

type int8'	= [`Int8]
type int16'	= [`Int16]
type int32'	= [`Int32]
type int64'	= [`Int64]

type natint'	= [`NatInt]

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
type 'a type' = type_repr
and ('a,'b) field' = field_repr
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
and type_repr = {
    t_name : string;
    t_requires : header list;
  }
and field_repr = {
    f_name : string;
    f_type : type_repr;
  }
and header = [ `Sys of string | `Usr of string ]

exception AlreadyDefined of string

let already_defined_exc fmt = Printf.ksprintf (fun s -> AlreadyDefined s) fmt

module TypeRepr =
  struct
    type t = type_repr

    let make ~name ~size ~align ~requires = {
      t_name		= name;
      t_requires	= requires;
    }

    let name t		= t.t_name
    let requires t	= t.t_requires
  end

module type TYPE =
    sig
      type w
      type t = w type'
	    
      val t : t
      val r : type_repr
    end

module type STD_TYPE_DESC =
    sig
      type w
      val name : ident
      val requires : header list
    end

module StdType (D : STD_TYPE_DESC) =
  struct
    type w = D.w
    type t = w type'
	  
    let t = {
      t_name = D.name;
      t_requires = D.requires;
    }
    let r = t
  end
    

module Int8 =
  StdType (struct
    type w = int8'
    let name = "int8_t"
    let requires = [ `Sys "stdint.h" ]
  end)
    
module Int16 =
  StdType (struct
    type w = int16'
    let name = "int16_t"
    let requires = [ `Sys "stdint.h" ]
  end)

module Int32 =
  StdType (struct
    type w = int32'
    let name = "int32_t"
    let requires = [ `Sys "stdint.h" ]
  end)

module Int64 =
  StdType (struct
    type w = int64'
    let name = "int64_t"
    let requires = [ `Sys "stdint.h" ]
  end)

module NatInt =
  StdType (struct
    type w = natint'
    let name = "natint"
    let requires = [ `Sys "caml/config.h" ]
  end)

module UInt8 =
  StdType (struct
    type w = uint8'
    let name = "uint8_t"
    let requires = [ `Sys "stdint.h" ]
  end)
      
module UInt16 =
  StdType (struct
    type w = uint16'
    let name = "uint16_t"
    let requires = [ `Sys "stdint.h" ]
  end)

module UInt32 =
  StdType (struct
    type w = uint32'
    let name = "uint32_t"
    let requires = [ `Sys "stdint.h" ]
  end)

module UInt64 =
  StdType (struct
    type w = uint64'
    let name = "uint64_t"
    let requires = [ `Sys "stdint.h" ]
  end)

module Bool =
  StdType (struct
    type w = bool'
    let name = "bool"
    let requires = [ `Sys "stdbool.h" ]
  end)

module Float32 =
  StdType (struct
    type w = float32'
    let name = "float"
    let requires = []
  end)

module Float64 =
  StdType (struct
    type w = float64'
    let name = "double"
    let requires = []
  end)

(*
module Struct (NewT : sig type t val name : ident end) :
    sig
      type t = NewT.t struct'
      type r = t Type.t
      type 'b f = (NewT.t,'b) field'

      val add_field : 'b Type.t -> ident -> 'b f

      val struct_repr : unit -> r
    end
    =
  struct
    type t = NewT.t struct'
    type r = t Type.t
    type 'b f = (NewT.t,'b) field'

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
*)
