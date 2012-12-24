include Zimt.MODULE

(* TODO : add definitions for caml/*.h interfaces *)
(* This should then allow us to build on that for creating the FFI *)

(*
type void = Zimt.unit'
type 'a struct' = 'a Zimt.struct'
type 'a x = 'a Zimt.x
type ('a,'b) field = ('a,'b) Zimt.field
type 'a v = 'a Zimt.camlvalue
type 'a ptr = 'a Zimt.ptr

type exception'
type string'	= Zimt.string'
type bool'	= Zimt.bool'
type mlsize_t	= Zimt.uintnat'
type tag_t	= Zimt.uint8'
type uintnat	= Zimt.uintnat'
type intnat	= Zimt.intnat'
type int	= Zimt.int'
type uint32	= Zimt.uint32'
type float64	= Zimt.float64'

module Alloc :
  sig
    val alloc		: mlsize_t x -> tag_t Zimt.x	-> 'a -> 'a v x
    val alloc_small	: mlsize_t x -> tag_t x		-> 'a -> 'a v x
    val alloc_tuple	: mlsize_t x			-> 'a -> 'a v x

    val alloc_string		: mlsize_t x			-> string v x
    val copy_string		: string' x			-> string v x
    val copy_string_array	: string' Zimt.ptr x		-> string array v x

    val copy_double	: Zimt.float64' x	-> float v x
    val copy_int32	: Zimt.int32' x		-> int32 v x
    val copy_int64	: Zimt.int64' x		-> int64 v x
    val copy_nativeint	: Zimt.natint' x	-> nativeint v x

    val alloc_array : ('a x -> 'a v x)	-> 'a ptr x -> 'a array v x

    val alloc_final : mlsize_t x -> ('a v x -> void x) -> mlsize_t x -> mlsize_t x -> 'a v x
  end

module Custom :
  sig
    type 'a ops
    type 'a ops_t = 'a ops struct'

    module type OPS =
      sig
	type custom
	include Zimt.TYPE with type w = custom ops_t

	type 'a field = (w,'a) Zimt.field

	type finalize_fn	= custom v x -> void x
	type compare_fn		= custom v x -> custom v x -> int x
	type hash_fn		= custom v x -> intnat x
	type serialize_fn	= custom v x -> uintnat ptr x -> uintnat ptr x -> void x
	type deserialize_fn	= custom ptr x -> uintnat ptr x
	type compare_ext_fn	= custom v x -> custom v x -> int x

	val identifier	: string		field
	val finalize	: finalize_fn		field
	val compare	: compare_fn		field
	val hash	: hash_fn		field
	val serialize	: serialize_fn		field
	val deserialize : deserialize_fn	field
	val compare_ext : compare_ext_fn	field
      end

    val make_ops : 'a Zimt.t -> (module OPS with type custom = 'a)

    val alloc_custom : 'a ops_t ptr x -> size:uintnat x -> mem:mlsize_t x -> max:mlsize_t x -> 'a v x

    val register_custom_operations : 'a ops_t ptr x -> void x
  end

module Memory :
  sig
    val store_field : 'a v x -> mlsize_t x -> 'b v x -> void x

    val register_global_root : 'a v ptr x -> void x
    val remove_global_root : 'a v ptr x -> void x
  end

module Hash :
  sig
    val hash_mix_uint32	: uint32 x -> uint32 x -> uint32 x
    val hash_mix_intnat	: uint32 x -> intnat x -> uint32 x
    val hash_mix_int64	: uint32 x -> int64 x -> uint32 x
    val hash_mix_double : uint32 x -> float64 x -> uint32 x
    val hash_mix_string : uint32 x -> string v x -> uint32 x
  end

module Fail :
  sig
    val failwith		: string' x -> void x
    val invalid_argument	: string' x -> void x
    val raise_out_of_memory	: void x
    val raise_stack_overflow	: void x
    val raise_sys_error		: void x
    val raise_end_of_file	: void x
    val raise_zero_divide	: void x
    val raise_not_found		: void x
  end

module Callback :
  sig
    val named_value : string' x -> 'a v ptr x

    val make_exception_result : exception' v x -> 'a v x
    val is_exception_result : 'a v x -> bool' x
    val extract_exception : 'a v x -> exception' v x
  end

module Threads :
  sig
    val aquire_runtime_system	: void x
    val release_runtime_system	: void x
  end
*)
