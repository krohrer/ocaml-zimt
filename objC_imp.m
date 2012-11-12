#include "caml/mlvalues.h"
#include "caml/custom.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/fail.h"

#include <stdio.h>
#include <memory.h>
#include <assert.h>

#include <objc/objc.h>
#include <objc/runtime.h>

// These are jut for testing and NSLog:
#include <Foundation/NSObjCRuntime.h>
#include <Foundation/NSValue.h>
// Do not include Foundation/Foundation.h directly -> type
// redefinition error: uint64.  And I have not yet found a way around
// it.

// Structs are custom values _________________________________________________

#define FFI_STRUCT_IDENTIFIER "ch.lambdamuesli.ffi.struct"
#define FFI_STRUCT_USE_MALLOC 1

struct FFI_struct {
  void *storage;
  void (*finalize)(void *);
};

static struct custom_operations FFI_struct_custom_ops;

#define FFI_struct_custom_val(v)		(*((struct FFI_struct*)Data_custom_val(v)))
#define FFI_struct_storage_custom_val(T, v)	(*((T*)FFI_struct_custom_val(v).storage))

value FFI_struct_alloc(size_t size, size_t alignment) {
#if FFI_STRUCT_USE_MALLOC
  // Use malloc to allocate storage for struct
  value v = caml_alloc_custom( &FFI_struct_custom_ops, sizeof(struct FFI_struct), 0, 1);
  struct FFI_struct *s = &FFI_struct_custom_val(v);
  s->storage = malloc(size);
  s->finalize = NULL;
  return v;
#else
  // Use custom block inplace storage
  const size_t alignment = 8;
  const size_t header = sizeof(FFI_struct);
  size_t total = header + size + alignment - 1;
  
  value v = caml_alloc_custom( &C_struct_custom_ops, total, 0, 1);
  void *data = Data_custom_val(v);
  struct FFI_struct *s = data;
  // Only works on 2-complement machines, but hey.
  s->storage = (void *)(((uintptr_t)data + header + alignment - 1) & -alignment);
  s->finalize = NULL;
  return v;
#endif
}

void FFI_struct_finalize(value v) {
  struct FFI_struct *s = &FFI_struct_custom_val(v);
  if (s->finalize != NULL) {
    s->finalize(s->storage);
  }
#if FFI_STRUCT_USE_MALLOC
  free(s->storage);
#endif
}

static struct custom_operations FFI_struct_custom_ops = {
  .identifier   = FFI_STRUCT_IDENTIFIER,
  .finalize	= FFI_struct_finalize,
  .compare	= custom_compare_default,
  .hash		= custom_hash_default,
  .serialize	= custom_serialize_default,
  .deserialize  = custom_deserialize_default,
  .compare_ext	= custom_compare_ext_default
};

// Enumeration types _________________________________________________________

value value_from_objc_AssociationPolicy(objc_AssociationPolicy policy) {
  if (policy == OBJC_ASSOCIATION_ASSIGN)		return 0;
  if (policy == OBJC_ASSOCIATION_RETAIN_NONATOMIC)	return 1;
  if (policy == OBJC_ASSOCIATION_COPY_NONATOMIC)	return 2;
  if (policy == OBJC_ASSOCIATION_RETAIN)		return 3;
  if (policy == OBJC_ASSOCIATION_COPY)			return 4;
  caml_failwith("ObjC: Unknown objc_AssociationPolicy. Check <objc/runtime.h>.");
}

objc_AssociationPolicy objc_AssociationPolicy_from_value(value vpolicy) {
  switch (Int_val(vpolicy)) {
  case 0:	return OBJC_ASSOCIATION_ASSIGN;
  case 1:	return OBJC_ASSOCIATION_RETAIN_NONATOMIC;
  case 2:	return OBJC_ASSOCIATION_COPY_NONATOMIC;
  case 3:	return OBJC_ASSOCIATION_RETAIN;
  case 4:	return OBJC_ASSOCIATION_COPY;
  }
  caml_failwith("ObjC: objc_AssociationPolicy mismatch. Check objC.mli.");
}

#define BOX_objc_AssociationPolicy(_, P)	value_from_objc_AssociationPolicy(P)
#define UNBOX_objc_AssociationPolicy(_, V)	objc_AssociationPolicy_from_value(V)

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

#define BOX_Class		FFI_BOX_PTR
#define BOX_Ivar		FFI_BOX_PTR
#define BOX_Method		FFI_BOX_PTR
#define BOX_SEL			FFI_BOX_PTR
#define BOX_IMP			FFI_BOX_PTR
#define BOX_id			FFI_BOX_PTR
#define BOX_objc_property_t	FFI_BOX_PTR

#define UNBOX_Class		FFI_UNBOX_PTR
#define UNBOX_Ivar		FFI_UNBOX_PTR
#define UNBOX_Method		FFI_UNBOX_PTR
#define UNBOX_SEL		FFI_UNBOX_PTR
#define UNBOX_IMP		FFI_UNBOX_PTR
#define UNBOX_id		FFI_UNBOX_PTR
#define UNBOX_objc_property_t	FFI_UNBOX_PTR

#define BOX_void(_, X)          (X, Val_unit)

#define BOX_BOOL(_, V)		Val_bool(V)
#define BOX_int(_, V)		Val_int(V)
#define BOX_int32(_, V)		caml_copy_int32(V)
#define BOX_int64(_, V)		caml_copy_int64(V)
#define BOX_intnat(_, V)	caml_copy_natiVeint(V)
#define BOX_double(_, V)	caml_copy_double(V)
#define BOX_size_t(_, V)	Val_int(V)
#define BOX_uint8_t(_, V)       Val_int((int)V)

#define UNBOX_BOOL(_, V)	Bool_val(V)
#define UNBOX_int(_, V)		Int_val(V)
#define UNBOX_int32(_, V)	Int32_val(V)
#define UNBOX_int64(_, V)	Int64_val(V)
#define UNBOX_intnat(_, V)	Nativeint_val(V)
#define UNBOX_double(_, V)	Double_val(V)
#define UNBOX_size_t(_, V)	Int_val(V)
#define UNBOX_uint8_t(_, V)     (uint8_t)Int_val(V)

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

// Helper macro to update values of type : ref int

#define Store_int_ref(vref, i) do { Field(vref, 0) = Val_int(i); } while (0)

/*
This should obviously only be used for values of type [int ref]!  This
works because ints are not (caml) heap pointers. Thus, we are not
changing anything of interest to the GC.  (if it doesn't, we only have
a single thing to/ fix!)
*/

#define FFI_COPY_LIST1(N, RT, CF)		\
  CAMLprim value N(value vcount_r) {		\
    unsigned int count = 0;			\
    RT list = CF(&count);			\
    Store_int_ref(vcount_r, count);		\
    return BOX_##RT(RT, list);			\
  }

#define FFI_COPY_LIST2(N, RT, CF, AT)		\
  CAMLprim value N(value a, value vcount_r) {	\
    unsigned int count = 0;			\
    RT list = CF(UNBOX_##AT(AT, a),		\
		 &count);			\
    Store_int_ref(vcount_r, count);		\
    return BOX_##RT(RT, list);			\
  }

#define FFI_COPY_LIST3(N, RT, CF, AT, BT)		\
  CAMLprim value N(value a, value b, value vcount_r) {	\
    unsigned int count = 0;				\
    RT list = CF(UNBOX_##AT(AT, a),			\
		 UNBOX_##BT(BT, b),			\
		 &count);				\
    Store_int_ref(vcount_r, count);			\
    return BOX_##RT(RT, list);				\
  }

#define FFI_COPY_LIST4(N, RT, CF, AT, BT, CT)			\
  CAMLprim value N(value a, value b, value c, value vcount_r) {	\
    unsigned int count = 0;					\
    RT list = CF(UNBOX_##AT(AT, a),				\
		 UNBOX_##AT(BT, b),				\
		 UNBOX_##AT(CT, c),				\
		 &count);					\
    Store_int_ref(vcount_r, count);				\
    return BOX_##RT(heapptr, list);				\
  }

// ObjC external functions ___________________________________________________

FFI1( C_malloc, PTR(void*), malloc, size_t )

FFI1( C_free, void, free, PTR(void*) )

FFI0( C_make_null, PTR(void*), NULL )

CAMLprim value C_dump_cstr(value vcstr) {
  printf("%s\n", FFI_UNBOX(PTR(const char*), vcstr));
  return Val_unit;
}

CAMLprim value C_copy_cstr(value vcstr) {
  return caml_copy_string(FFI_UNBOX(PTR(const char*), vcstr));
}

CAMLprim value C_ptr_array_ith(value vparr, value vi) {
  void **parr = FFI_UNBOX(PTR(void**), vparr);
  int i = FFI_UNBOX(int, vi);
  void *p = parr[i];
  return FFI_BOX(PTR(void *), p);
}

//_____________________________________

FFI1( Class_get_name,
      PTR(const char*), class_getName, Class)

FFI1( Class_is_meta_class,
      BOOL, class_isMetaClass, Class)

FFI1( Class_get_superclass,
      Class, class_getSuperclass, Class )

FFI1( Class_get_version,
      int32, class_getVersion, Class )

FFI2( Class_set_version,
      void, class_setVersion, Class, int32 )

FFI1( Class_get_instance_size,
      size_t, class_getInstanceSize, Class )

FFI2( Class_get_instance_variable,
      Ivar, class_getInstanceVariable, Class, PTR(const char*) )

FFI2( Class_get_class_variable,
      Ivar, class_getClassVariable, Class, PTR(const char*) )

FFI_COPY_LIST2( Class_copy_ivar_list,
		PTR(Ivar *), class_copyIvarList, Class )

FFI2( Class_get_instance_method,
      Method, class_getInstanceMethod, Class, SEL )

FFI2( Class_get_class_method,
      Method, class_getClassMethod, Class, SEL )

FFI2( Class_get_method_implementation,
      IMP, class_getMethodImplementation, Class, SEL )

FFI2( Class_get_method_implementation_stret,
      IMP, class_getMethodImplementation_stret, Class, SEL )

FFI2( Class_responds_to_selector,
      BOOL, class_respondsToSelector, Class, SEL )

FFI_COPY_LIST2( Class_copy_method_list,
		PTR(Method*), class_copyMethodList, Class )

FFI2( Class_conforms_to_protocol,
      BOOL, class_conformsToProtocol, Class, PTR(Protocol*) )

FFI_COPY_LIST2( Class_copy_protocol_list,
		PTR(Protocol**),
		class_copyProtocolList,
		Class )

FFI2( Class_get_property,
      objc_property_t, class_getProperty, Class, PTR(const char*) )

FFI_COPY_LIST2( Class_copy_property_list,
		PTR(objc_property_t*),
		class_copyPropertyList,
		Class )

FFI1( Class_get_ivar_layout,
      PTR(const uint8_t*), class_getIvarLayout, Class )

FFI1( Class_get_weak_ivar_layout,
      PTR(const uint8_t*), class_getWeakIvarLayout, Class )

FFI2( Class_create_instance,
      id, class_createInstance, Class, size_t )

FFI4( Class_add_method,
      BOOL, class_addMethod, Class, SEL, IMP, PTR(const char*) )

FFI4( Class_replace_method,
      IMP, class_replaceMethod, Class, SEL, IMP, PTR(const char*) )

FFI5( Class_add_ivar,
      BOOL,
      class_addIvar,
      Class, PTR(const char*), size_t, uint8_t, PTR(const char*) )

FFI2( Class_add_protocol,
      BOOL, class_addProtocol, Class, PTR(Protocol*) )

FFI4( Class_add_property,
      BOOL,
      class_addProperty,
      Class, PTR(const char*), PTR(const objc_property_attribute_t*), int )

FFI4( Class_replace_property,
      void,
      class_replaceProperty,
      Class, PTR(const char*), PTR(const objc_property_attribute_t*), int )

FFI2( Class_set_ivar_layout,
      void, class_setIvarLayout, Class, PTR(const uint8_t*) )

FFI2( Class_set_weak_ivar_layout,
      void, class_setWeakIvarLayout, Class, PTR(const uint8_t*) )

FFI1( Class_get_image_name,
      PTR(const char*), class_getImageName, Class )

//_____________________________________

FFI1( Method_get_name,
      SEL, method_getName, Method )

FFI1( Method_get_implementation,
      IMP, method_getImplementation, Method )

FFI1( Method_get_type_encoding,
      PTR(const char*), method_getTypeEncoding, Method )

FFI1( Method_get_number_of_arguments,
      int, method_getNumberOfArguments, Method )

FFI1( Method_copy_return_type,
      PTR(const char*), method_copyReturnType, Method )

FFI2( Method_copy_argument_type,
      PTR(const char*), method_copyArgumentType, Method, int )

FFI1( Method_get_description,
      PTR(struct objc_method_description*), method_getDescription, Method )

FFI2( Method_set_implementation,
      IMP, method_setImplementation, Method, IMP )

FFI2( Method_exchange_implementations,
      void, method_exchangeImplementations, Method, Method )

//_____________________________________

FFI1( Ivar_get_name,
      void, ivar_getName, Ivar )

FFI1( Ivar_get_type_encoding,
      PTR(const char*), ivar_getTypeEncoding, Ivar )

FFI1( Ivar_get_offset,
      int, ivar_getOffset, Ivar )

//_____________________________________

FFI1( Property_get_name,
      PTR(const char*), property_getName, objc_property_t )

FFI1( Property_get_attributes,
      PTR(const char*), property_getAttributes, objc_property_t )

FFI_COPY_LIST2( Property_copy_attribute_list,
		PTR(objc_property_attribute_t*),
		property_copyAttributeList,
		objc_property_t )

FFI2( Property_copy_attribute_value,
      PTR(char*), property_copyAttributeValue, objc_property_t, PTR(const char*) )

//_____________________________________

FFI2( Protocol_conforms_to_protocol,
      BOOL, protocol_conformsToProtocol, PTR(Protocol*), PTR(Protocol*) )

FFI2( Protocol_is_equal,
      BOOL, protocol_isEqual, PTR(Protocol*), PTR(Protocol*) )

FFI1( Protocol_get_name,
      PTR(const char*), protocol_getName, PTR(Protocol*) )

FFI4( Protocol_get_method_description,
      STRUCT(struct objc_method_description),
      protocol_getMethodDescription,
      PTR(Protocol*), SEL, BOOL, BOOL)

FFI_COPY_LIST4( Protocol_copy_method_description_list,
		PTR(struct objc_method_description*),
		protocol_copyMethodDescriptionList,
		PTR(Protocol*), BOOL, BOOL )

FFI4( Protocol_get_property,
      objc_property_t,
      protocol_getProperty,
      PTR(Protocol*), PTR(const char*), BOOL, BOOL )

FFI_COPY_LIST2( Protocol_copy_property_list,
		PTR(objc_property_t*),
		protocol_copyPropertyList,
		PTR(Protocol*) )

FFI_COPY_LIST2( Protocol_copy_protocol_list,
		PTR(Protocol **),
		protocol_copyProtocolList,
		PTR(Protocol*) )

FFI5( Protocol_add_method_description,
      void,
      protocol_addMethodDescription,
      PTR(Protocol*), SEL, PTR(const char*), BOOL, BOOL )

FFI2( Protocol_add_protocol,
      void,
      protocol_addProtocol,
      PTR(Protocol*), PTR(Protocol*) )

FFI6( Protocol_add_property,
      void,
      protocol_addProperty,
      PTR(Protocol*),
      PTR(const char*),
      PTR(const objc_property_attribute_t*),
      int,
      BOOL, 
      BOOL )

//_____________________________________

FFI1( Sel_get_name,
      PTR(const char*), sel_getName, SEL )

FFI1( Sel_get_uid,
      SEL, sel_getUid, PTR(const char*) )

FFI1( Sel_register_name,
      SEL, sel_registerName, PTR(const char*) )

FFI2( Sel_is_equal,
      BOOL, sel_isEqual, SEL, SEL )

//_____________________________________

FFI2( Object_copy,
      id, object_copy, id, size_t )

FFI1( Object_dispose,
      id, object_dispose, id )

FFI1( Object_get_class,
      Class, object_getClass, id )

FFI2( Object_set_class,
      Class, object_setClass, id, Class )

FFI1( Object_get_class_name,
      PTR(const char*), object_getClassName, id )

FFI1( Object_get_indexed_ivars,
      PTR(void*), object_getIndexedIvars, id )

FFI2( Object_get_ivar,
      id, object_getIvar, id, Ivar )

FFI3( Object_set_ivar,
      void, object_setIvar, id, Ivar, id )

FFI3( Object_set_instance_variable,
      Ivar, object_setInstanceVariable, id, PTR(const char*), PTR(void*) )

CAMLprim value Object_get_instance_variable(value vobj, value vname, value vout_r) {
  // If you modify this function without knowing exactly what you are
  // doing, consider adding CAMLparam / CAMLlocal / CAMLreturn macros.
  void *outValue = NULL;
  Ivar ret = object_getInstanceVariable(FFI_UNBOX(id, vobj),
					FFI_UNBOX(PTR(const char*), vname),
					&outValue);
  Store_field(vout_r, 0, FFI_BOX(PTR(void*), outValue));
  // Do not use any values anymore. They could have been invalidated
  // by the GC.
  return FFI_BOX(Ivar, ret);
}

//_____________________________________

FFI2( ObjC_construct_instance,
      id, objc_constructInstance, Class, PTR(void*) )

FFI1( ObjC_destruct_instance,
      PTR(void*), objc_destructInstance, id )

FFI3( ObjC_allocate_class_pair,
      Class, objc_allocateClassPair, Class, PTR(const char*), size_t )

FFI1( ObjC_register_class_pair,
      void, objc_registerClassPair, Class )

FFI3( ObjC_duplicate_class,
      Class, objc_duplicateClass, Class, PTR(const char*), size_t )

FFI1( ObjC_dispose_class_pair,
      void, objc_disposeClassPair, Class )

FFI_COPY_LIST1( ObjC_copy_image_names,
		PTR(const char**), objc_copyImageNames )

FFI_COPY_LIST2( ObjC_copy_class_names_for_image,
		PTR(const char**), objc_copyClassNamesForImage, PTR(const char*) )

FFI1( ObjC_allocate_protocol,
      PTR(Protocol *), objc_allocateProtocol, PTR(const char*) )

FFI1( ObjC_register_protocol,
      void, objc_registerProtocol, PTR(Protocol*) )

FFI4( ObjC_set_associated_object,
      void, objc_setAssociatedObject,
      id, PTR(const char*), id, objc_AssociationPolicy )

FFI2( ObjC_get_associated_object,
      id, objc_getAssociatedObject, id, PTR(const char*) )

FFI1( ObjC_remove_associated_objects,
      void, objc_removeAssociatedObjects, id )

FFI1( ObjC_get_class,
      id, objc_getClass, PTR(const char*) )

FFI1( ObjC_get_meta_class,
      id, objc_getMetaClass, PTR(const char*) )

FFI1( ObjC_look_up_class,
      id, objc_lookUpClass, PTR(const char*) )

FFI1( ObjC_get_required_class,
      id, objc_getRequiredClass, PTR(const char*) )

FFI1( ObjC_get_future_class,
      id, objc_getFutureClass, PTR(const char*) )

FFI2( ObjC_set_future_class,
      void, objc_setFutureClass, Class, PTR(const char*) )

FFI2( ObjC_get_class_list,
      int, objc_getClassList, PTR(Class*), int )

FFI_COPY_LIST1( ObjC_copy_class_list,
		PTR(Class*), objc_copyClassList )

FFI1( ObjC_get_protocol,
      PTR(Protocol*), objc_getProtocol, PTR(const char*) )

FFI_COPY_LIST1( ObjC_copy_protocol_list,
		PTR(Protocol**), objc_copyProtocolList )

//____________________________________________________________________________
