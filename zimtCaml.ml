open Printf

include (ZimtModule.Make(struct
  let name = "ZimtForeign"
  let includes =
    let caml n = `Usr (sprintf "caml/%s.h" n) in
    [
      caml "mlvalues";
      caml "custom";
      caml "alloc";
      caml "memory";
      caml "callback";
      caml "fail"
  ]
end))

type void = Zimt.void'
type 'a struct' = 'a Zimt.struct'
type 'a x = 'a Zimt.x
type ('a,'b) field = ('a,'b) Zimt.field
type 'a value = 'a Caml.t
type poly_val = Caml.poly value
type 'a ptr = 'a Zimt.ptr

type mlsize_t'	= Zimt.uintnat'
type tag_t'	= Zimt.uint8'

let mlsize_t = Zimt.TPrim Zimt.UIntNat
let tag_t = Zimt.TPrim Zimt.UInt8

let string' = Zimt.TPrim Zimt.String
let ptr t = Zimt.TPtr t

open Zimt

module Alloc =
  struct
    let valarg ct n = Fn.arg (TCaml ct) n
    let valret ct = Fn.ret (TCaml ct)
    
    let alloc = extern' "caml_alloc"
	Fn.(arg mlsize_t "size" ^^ arg tag_t "tag" ^^ valret Caml.Poly)

    let alloc_small = extern' "caml_alloc_small"
      Fn.(arg mlsize_t "size" ^^ arg tag_t "tag" ^^ valret Caml.Poly)

    let alloc_tuple = extern' "caml_alloc_tuple"
      Fn.(arg mlsize_t "size" ^^ valret Caml.Poly)

    let alloc_string = extern' "caml_alloc_string"
      Fn.(arg mlsize_t "size" ^^ valret Caml.String)

    let copy_string = extern' "caml_copy_string"
      Fn.(arg string' "str" ^^ valret Caml.String)

    let copy_string_array = extern' "caml_copy_string_array"
      Fn.(arg (ptr string') "strings" ^^ valret (Caml.Array Caml.String))

    let copy_double = extern' "caml_copy_double"
      Fn.(arg (TPrim Float64) "d" ^^ valret Caml.Float)

    let copy_int32 = extern' "caml_copy_int32"
      Fn.(arg (TPrim Int32) "i" ^^ valret Caml.Int32)

    let copy_int64 = extern' "caml_copy_int64"
      Fn.(arg (TPrim Int64) "i" ^^ valret Caml.Int64)

    let copy_nativeint = extern' "caml_copy_nativeint"
      Fn.(arg (TPrim IntNat) "i" ^^ valret Caml.Nativeint)
  end
