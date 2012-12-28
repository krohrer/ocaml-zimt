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

type void' = Zimt.void'
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
let sptr t = Zimt.TPtr (Zimt.PStatic t)

open Zimt

module Alloc =
  struct
    let valarg ct n = Fn.arg (TCaml ct) n
    let valret ct = Fn.ret (TCaml ct)
    
    let alloc, _ = extern' "caml_alloc"
	Fn.(uarg mlsize_t ^^ uarg tag_t ^^ valret Caml.Poly)

    let alloc_small, _ = extern' "caml_alloc_small"
      Fn.(uarg mlsize_t ^^ uarg tag_t ^^ valret Caml.Poly)

    let alloc_tuple, _ = extern' "caml_alloc_tuple"
      Fn.(uarg mlsize_t ^^ valret Caml.Poly)

    let alloc_string, _ = extern' "caml_alloc_string"
      Fn.(uarg mlsize_t ^^ valret Caml.String)

    let copy_string, _ = extern' "caml_copy_string"
      Fn.(uarg string' ^^ valret Caml.String)

    let copy_string_array, _ = extern' "caml_copy_string_array"
      Fn.(uarg (sptr string') ^^ valret (Caml.Array Caml.String))

    let copy_double, _ = extern' "caml_copy_double"
      Fn.(uarg (TPrim Float64) ^^ valret Caml.Float)

    let copy_int32, _ = extern' "caml_copy_int32"
      Fn.(uarg (TPrim Int32) ^^ valret Caml.Int32)

    let copy_int64, _ = extern' "caml_copy_int64"
      Fn.(uarg (TPrim Int64) ^^ valret Caml.Int64)

    let copy_nativeint, _ = extern' "caml_copy_nativeint"
      Fn.(uarg (TPrim IntNat) ^^ valret Caml.Nativeint)
  end
