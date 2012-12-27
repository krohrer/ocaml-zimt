include Zimt.MODULE

(* TODO : add definitions for caml/*.h interfaces *)
(* This should then allow us to build on that for creating the FFI *)

type void = Zimt.void'
type 'a struct' = 'a Zimt.struct'
type 'a x = 'a Zimt.x
type ('a,'b) field = ('a,'b) Zimt.field
type 'a value = 'a Caml.t
type poly_val = Caml.poly value
type 'a ptr = 'a Zimt.ptr

type mlsize_t'	= Zimt.uintnat'
type tag_t'	= Zimt.uint8'

module Alloc :
  sig
    val alloc		: mlsize_t' x -> tag_t' x	-> poly_val x
    val alloc_small	: mlsize_t' x -> tag_t' x	-> poly_val x
    val alloc_tuple	: mlsize_t' x			-> poly_val x

    val alloc_string		: mlsize_t' x		-> string value x
    val copy_string		: Zimt.string' x	-> string value x
    val copy_string_array	: Zimt.string' ptr x	-> string array value x

    val copy_double	: Zimt.float64' x	-> float value x
    val copy_int32	: Zimt.int32' x		-> int32 value x
    val copy_int64	: Zimt.int64' x		-> int64 value x
    val copy_nativeint	: Zimt.intnat' x	-> nativeint value x

    (* val alloc_array : ('r,'a x->('b value x as 'r)) Zimt.fn x -> 'a ptr x -> 'b array value x *)

    (* val alloc_final : mlsize_t' x -> ('r,'a value x->(void x as 'r)) Zimt.fn x -> mlsize_t' x -> mlsize_t' x -> 'a value x *)
  end

(*
module Custom :
  sig
    type 'a ops
    type 'a ops_t = 'a ops struct'

    module type OPS =
      sig
	type custom
	include Zimt.TYPE with type w = custom ops_t

	type 'a field = (w,'a) Zimt.field

	type finalize_fn	= custom value x -> void x
	type compare_fn		= custom value x -> custom value x -> int x
	type hash_fn		= custom value x -> Zimt.intnat' x
	type serialize_fn	= custom value x -> Zimt.uintnat' ptr x -> Zimt.uintnat' ptr x -> void x
	type deserialize_fn	= custom ptr x -> Zimt.uintnat' ptr x
	type compare_ext_fn	= custom value x -> custom value x -> int x

	val identifier	: string		field
	val finalize	: finalize_fn		field
	val compare	: compare_fn		field
	val hash	: hash_fn		field
	val serialize	: serialize_fn		field
	val deserialize : deserialize_fn	field
	val compare_ext : compare_ext_fn	field
      end

    val make_ops : 'a Zimt.t -> (module OPS with type custom = 'a)

    val alloc_custom :
      'a ops_t ptr x -> size:Zimt.uintnat' x ->
      mem:mlsize_t' x -> max:mlsize_t' x -> 'a value x

    val register_custom_operations : 'a ops_t ptr x -> void x
  end

module Memory :
  sig
    val store_field : 'a value x -> mlsize_t' x -> 'b value x -> void x

    val register_global_root	: 'a value ptr x -> void x
    val remove_global_root	: 'a value ptr x -> void x
  end

module Hash :
  sig
    type uint32' = Zimt.uint32'

    val hash_mix_uint32	: uint32' x -> Zimt.uint32'  x -> uint32' x
    val hash_mix_intnat	: uint32' x -> Zimt.intnat'  x -> uint32' x
    val hash_mix_int64	: uint32' x -> Zimt.int64'   x -> uint32' x
    val hash_mix_double : uint32' x -> Zimt.float64' x -> uint32' x
    val hash_mix_string : uint32' x -> Zimt.string' value x -> uint32' x
  end

module Fail :
  sig
    val failwith		: Zimt.string' x -> void x
    val invalid_argument	: Zimt.string' x -> void x
    val raise_out_of_memory	: void x
    val raise_stack_overflow	: void x
    val raise_sys_error		: void x
    val raise_end_of_file	: void x
    val raise_zero_divide	: void x
    val raise_not_found		: void x
  end

module Intex :
  sig
    val serialize_int_1 : Zimt.int8' x -> void x
    val serialize_int_2 : Zimt.int16' x -> void x
    val serialize_int_4 : Zimt.int32' x -> void x
    val serialize_int_8 : Zimt.int64' x -> void x
  end

module Callback :
  sig
    val named_value : Zimt.string' x -> 'a value ptr x

    val make_exception_result	: exn value x -> 'a value x
    val is_exception_result	: 'a value x -> Zimt.bool' x
    val extract_exception	: 'a value x -> exn value x
  end

module Threads :
  sig
    val aquire_runtime_system	: void x
    val release_runtime_system	: void x
  end
*)
