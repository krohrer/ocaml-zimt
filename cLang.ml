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
type ('a,'b) fun' = 'a -> 'b

(* C Language description *)
type 'a type' = type_repr
and ('a,'b) field' = field_repr
and _ x =
| XLiteral	: 'a literal				-> 'a x
| XUnsafeFun	: ('a*'b) literal			-> ('a,'b) fun' x
| XVar		: 'a var				-> 'a x
| XOp1		: ('a,'b) op1 * 'a x 			-> 'b x
| XOp2		: ('a,'b,'c) op2 * 'a x * 'b x 		-> 'c x
| XDeref	: 'a ptr' x				-> 'a x
| XField	: 'a x * ('a,'b) field'			-> 'b x
| XArrSubs	: 'a ptr' x * int' x			-> 'a x
| XCall		: ('a,'b) fun' x * 'a x			-> 'b x
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
and type_repr = {
    t_name : string;
    t_defined : bool;
    t_requires : header list;
    t_metatype : metatype;
  }
and field_repr = ident * type_repr
and header = [ `Sys of string | `Usr of string ]
and metatype =
| MTScalar
| MTStruct of field_repr list

(* Exceptions *)
exception AlreadyDefined of string

let already_defined_exc fmt = Printf.ksprintf (fun s -> AlreadyDefined s) fmt

module TypeRepr =
  struct
    type t = type_repr

    let name t		= t.t_name
    let defined t	= t.t_defined
    let requires t	= t.t_requires
  end

module type TYPE =
    sig
      type t

      val t : t type'
      val r : type_repr
    end

module type TYPE_DESC =
    sig
      type t
      val name : ident
      val defined : bool
      val requires : header list
    end

(* Create new scalar types *)
module ScalarType (D : TYPE_DESC) : TYPE with type t = D.t =
  struct
    type t = D.t

    let t = {
      t_name = D.name;
      t_defined = D.defined;
      t_requires = D.requires;
      t_metatype = MTScalar;
    }
    let r = t
  end
    
module Int8 =
  ScalarType (struct
    type t = int8'
    let name = "int8_t"
    let defined = true
    let requires = [ `Sys "stdint.h" ]
  end)
    
module Int16 =
  ScalarType (struct
    type t = int16'
    let name = "int16_t"
    let defined = true
    let requires = [ `Sys "stdint.h" ]
  end)

module Int32 =
  ScalarType (struct
    type t = int32'
    let name = "int32_t"
    let defined = true
    let requires = [ `Sys "stdint.h" ]
  end)

module Int64 =
  ScalarType (struct
    type t = int64'
    let name = "int64_t"
    let defined = true
    let requires = [ `Sys "stdint.h" ]
  end)

module NatInt =
  ScalarType (struct
    type t = natint'
    let name = "natint"
    let defined = true
    let requires = [ `Sys "caml/config.h" ]
  end)

module UInt8 =
  ScalarType (struct
    type t = uint8'
    let name = "uint8_t"
    let defined = true
    let requires = [ `Sys "stdint.h" ]
  end)
      
module UInt16 =
  ScalarType (struct
    type t = uint16'
    let name = "uint16_t"
    let defined = true
    let requires = [ `Sys "stdint.h" ]
  end)

module UInt32 =
  ScalarType (struct
    type t = uint32'
    let name = "uint32_t"
    let defined = true
    let requires = [ `Sys "stdint.h" ]
  end)

module UInt64 =
  ScalarType (struct
    type t = uint64'
    let name = "uint64_t"
    let defined = true
    let requires = [ `Sys "stdint.h" ]
  end)

module Bool =
  ScalarType (struct
    type t = bool'
    let name = "bool"
    let defined = true
    let requires = [ `Sys "stdbool.h" ]
  end)

module Float32 =
  ScalarType (struct
    type t = float32'
    let name = "float"
    let defined = true
    let requires = []
  end)

module Float64 =
  ScalarType (struct
    type t = float64'
    let name = "double"
    let defined = true
    let requires = []
  end)

module StructMixin (D : TYPE_DESC) :
    sig
      val add_field : 'a type' -> ident -> (D.t struct','a) field'
    
      val make_type : unit -> D.t struct' type'
      val make_repr : unit -> TypeRepr.t
    end
    =
  struct
    let sealed = ref false
    let fields = ref []

    let add_field ft id =
      if !sealed then
	raise (already_defined_exc "struct %s: cannot add field %s after make_type ()" D.name id)
      else if List.mem_assoc id !fields then
	raise (already_defined_exc "struct %s: field %s" D.name id)
      else begin
	let f = (id, ft) in
	fields := f :: !fields;
	f
      end

    let make_type () = 
      sealed := true;
      {
       t_name = D.name;
       t_defined = D.defined;
       t_requires = D.requires;
       t_metatype = MTStruct !fields;
     }

    let make_repr () = make_type ()
  end

module CustomStruct =
  struct
    type s
    type t = s struct'
    type 'a f = (t,'a) field'

    include StructMixin
	(struct
	  type t = s
	  let name = "Hello"
	  let defined = false
	  let requires = [ `Usr "hello.h" ]
	end)

    let some_field = add_field Int8.t "someField"
    let other_field = add_field Float32.t "otherField"

    let t = make_type ()
    let r = make_repr ()
  end

module AnotherStruct =
  struct
    type s
    type t = s struct'
    type 'a f = (t,'a) field'

    include StructMixin
	(struct
	  type t = s
	  let name = "Hello"
	  let defined = false
	  let requires = [ `Usr "hello.h" ]
	end)

    let some_field = add_field Int8.t "someField"
    let other_field = add_field Float32.t "otherField"

    let t = make_type ()
    let r = make_repr ()
end
