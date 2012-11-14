#ifndef INCLUDE___CAMLFFI_H
#define INCLUDE___CAMLFFI_H

#include "caml/mlvalues.h"
#include "caml/custom.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/fail.h"

#include <stdio.h>
#include <memory.h>

// Structs are custom values _________________________________________________

#define FFI_STRUCT_IDENTIFIER "ch.lambdamuesli.ffi.struct"
#define FFI_STRUCT_USE_MALLOC 1

struct FFI_struct {
  void *storage;
  void (*finalize)(void *);
};

#define FFI_struct_custom_val(v)		(*((struct FFI_struct*)Data_custom_val(v)))
#define FFI_struct_storage_custom_val(T, v)	(*((T*)FFI_struct_custom_val(v).storage))

value FFI_struct_alloc(size_t size, size_t alignment);

// PP magic __________________________________________________________________

// http://coding.derkeiler.com/Archive/C_CPP/comp.lang.c/2010-07/msg00515.html
// Pretty cool, eh? Does not work for everything, but certainly works for structs
#define alignmentof(T) offsetof(struct {char foo;T bar;}, bar)

// Conversion to/from OCaml values

// Helper macros
// Defined such that UNWRAP(PTR(T)) works

#define FFI_BOX_PTR(_, P)	((value)P)
#define FFI_UNBOX_PTR(T, V)	((T)V)

// Gotta love gcc extensions!
#define FFI_BOX_STRUCT(T, S)					\
  ({								\
    value v = FFI_struct_alloc(sizeof(T), alignmentof(T));	\
    FFI_struct_storage_custom_val(T, v) = S;			\
    v;								\
  })
#define FFI_UNBOX_STRUCT(T, V)	(*((T*)FFI_struct_storage_custom_val(V)))

#define BOX_PTR(_)		FFI_BOX_PTR
#define UNBOX_PTR(_)		FFI_UNBOX_PTR

#define BOX_STRUCT(_)		FFI_BOX_STRUCT
#define UNBOX_STRUCT(_)		FFI_UNBOX_STRUCT

#define PTR(T)		        T
#define STRUCT(T)	        T

// Only use those outside of macro contexts
#define FFI_BOX(T, V)           BOX_##T(T, V)
#define FFI_UNBOX(T, V)         UNBOX_##T(T, V)

#define BOX_void(_, X)          (X, Val_unit)

// Only caml types
#define BOX_char(_, V)          Val_int(V)
#define BOX_bool(_, V)          Val_bool(V)
#define BOX_int(_, V)		Val_int(V)
#define BOX_int32(_, V)		caml_copy_int32(V)
#define BOX_int64(_, V)		caml_copy_int64(V)
#define BOX_intnat(_, V)	caml_copy_natiVeint(V)
#define BOX_double(_, V)	caml_copy_double(V)

#define UNBOX_char(_, V)        Int_val(V)
#define UNBOX_bool(_, V)        Bool_val(V)
#define UNBOX_int(_, V)		Int_val(V)
#define UNBOX_int32(_, V)	Int32_val(V)
#define UNBOX_int64(_, V)	Int64_val(V)
#define UNBOX_intnat(_, V)	Nativeint_val(V)
#define UNBOX_double(_, V)	Double_val(V)

// Easily wrap simple C functions 
// We cannot directly BOX the return value, because structs!
// But we cannot 

#define FFI0(N, RT, EXPR)			\
  CAMLprim value N(value unit) {		\
    return BOX_##RT(RT, EXPR);			\
  }

#define FFI1(N, RT, CF, AT)			\
  CAMLprim value N(value a) {			\
    return BOX_##RT(RT, CF(UNBOX_##AT(AT, a)));	\
  }

#define FFI2(N, RT, CF, AT, BT)			\
  CAMLprim value N(value a, value b) {		\
    return BOX_##RT(RT, CF(UNBOX_##AT(AT, a),	\
			   UNBOX_##BT(BT, b)));	\
  }

#define FFI3(N, RT, CF, AT, BT, CT)		\
  CAMLprim value N(value a, value b, value c) {	\
    return BOX_##RT(RT, CF(UNBOX_##AT(AT, a),	\
			   UNBOX_##BT(BT, b),	\
			   UNBOX_##CT(CT, c)));	\
  }

#define FFI4(N, RT, CF, AT, BT, CT, DT)				\
  CAMLprim value N(value a, value b, value c, value d) {	\
    return BOX_##RT(RT, CF(UNBOX_##AT(AT, a),			\
			   UNBOX_##BT(BT, b),			\
			   UNBOX_##CT(CT, c),			\
			   UNBOX_##DT(DT, d)));			\
  }

#define FFI5(N, RT, CF, AT, BT, CT, DT, ET)				\
  CAMLprim value N(value a, value b, value c, value d, value e) {	\
    return BOX_##RT(RT, CF(UNBOX_##AT(AT, a),				\
			   UNBOX_##BT(BT, b),				\
			   UNBOX_##CT(CT, c),				\
			   UNBOX_##DT(DT, d),				\
			   UNBOX_##ET(ET, e)));				\
  }

// Functions with more than 5 arguments need special entry points for
// bytecode!

#define FFI6(N, RT, CF, AT, BT, CT, DT, ET, FT)				\
  CAMLprim value N##__native(value a,					\
			     value b,					\
			     value c,					\
			     value d,					\
			     value e,					\
			     value f) {					\
    return BOX_##RT(RT, CF(UNBOX_##AT(AT, a),				\
			   UNBOX_##BT(BT, b),				\
			   UNBOX_##CT(CT, c),				\
			   UNBOX_##DT(DT, d),				\
			   UNBOX_##ET(ET, e),				\
			   UNBOX_##FT(FT, f)));				\
  }									\
  CAMLprim value N##__bytecode(value *v, int argn) {			\
    return N##__native(v[0], v[1], v[2], v[3], v[4], v[5]);		\
  }

#define FFI7(N, RT, CF, AT, BT, CT, DT, ET, FT, GT)			\
  CAMLprim value N##__native(value a,					\
			     value b,					\
			     value c,					\
			     value d,					\
			     value e,					\
			     value f,					\
			     value g) {					\
    return BOX_##RT(RT, CF(UNBOX_##AT(AT, a),				\
			   UNBOX_##BT(BT, b),				\
			   UNBOX_##CT(CT, c),				\
			   UNBOX_##DT(DT, d),				\
			   UNBOX_##ET(ET, e),				\
			   UNBOX_##FT(FT, f),				\
			   UNBOX_##GT(GT, g)));				\
  }									\
  CAMLprim value N##__bytecode(value *v, int argn) {			\
    return N##__native(v[0], v[1], v[2], v[3], v[4], v[5], v[6]);	\
  }

#endif
