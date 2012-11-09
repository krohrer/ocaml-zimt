#include "caml/mlvalues.h"
#include "caml/custom.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/callback.h"

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

// PP magic __________________________________________________________________

// Conversion to/from OCaml values

#define heapptr_to_value(PTR)      ((value)(PTR))
#define heapptr_from_value(T, VAL) ((T)(VAL))
#define pointer_to_value(PTR)      ((value)(PTR))
#define pointer_from_value(T, VAL) ((T)(VAL))

#define voidptr_to_value pointer_to_value
#define Class_to_value pointer_to_value
#define Ivar_to_value pointer_to_value
#define cstr_to_value pointer_to_value
#define Method_to_value pointer_to_value
#define SEL_to_value pointer_to_value
#define IMP_to_value pointer_to_value
#define id_to_value pointer_to_value
#define Protocol_ptr_to_value id_to_value

#define voidptr_from_value(V) pointer_from_value(void *, V)
#define Class_from_value(V) pointer_from_value(Class, V)
#define Ivar_from_value(V) pointer_from_value(Ivar, V)
#define cstr_from_value(V) pointer_from_value(const char *, V)
#define Method_from_value(V) pointer_from_value(Method, V)
#define SEL_from_value(V) pointer_from_value(SEL, V)
#define IMP_from_value(v) pointer_from_value(IMP, V)
#define id_from_value(V) pointer_from_value(id, V)
#define Protocol_ptr_from_value(V) pointer_from_value(Protocol *, V)

#define void_to_value(EXPR) ((EXPR), Val_unit)

#define BOOL_to_value Val_bool
#define int_to_value Val_int
#define int32_to_value caml_copy_int32
#define int64_to_value caml_copy_int64
#define intnat_to_value caml_copy_nativeint
#define double_to_value caml_copy_double
#define size_t_to_value Val_int

#define BOOL_from_value Bool_val
#define int_from_value Int_val
#define int32_from_value Int32_val
#define int64_from_value Int64_from_value
#define intnat_from_value Nativeint_val
#define double_from_value Double_val
#define size_t_from_value Int_val

// Helper macros

#define WRAP(T, EXPR) T##_to_value(EXPR)
#define UNWRAP(T, EXPR) T##_from_value(EXPR)

// Easily wrap simple C functions 

#define FFI0(N, RT, EXPR)			\
  value N(value unit) {				\
    return WRAP(RT, EXPR);			\
  }
#define FFI1(N, RT, CF, AT)			\
  value N(value a) {				\
    return WRAP(RT, CF(UNWRAP(AT, a)));		\
  }
#define FFI2(N, RT, CF, AT, BT)		\
  value N(value a, value b) {			\
    return WRAP(RT, CF(UNWRAP(AT, a),		\
		       UNWRAP(BT, b)));		\
  }
#define FFI3(N, RT, CF, AT, BT, CT)		\
  value N(value a, value b, value c) {		\
    return WRAP(RT, CF(UNWRAP(AT, a),		\
		       UNWRAP(BT, b),		\
		       UNWRAP(CT, c)));		\
  }
#define FFI4(N, RT, CF, AT, BT, CT, DT)	\
  value N(value a, value b, value c, value d) {	\
    return WRAP(RT, CF(UNWRAP(AT, a),		\
		       UNWRAP(BT, b),		\
		       UNWRAP(CT, c),		\
		       UNWRAP(DT, d)));		\
  }
#define FFI5(N, RT, CF, AT, BT, CT, DT, ET)			\
  value N(value a, value b, value c, value d, value e) {	\
    return WRAP(RT, CF(UNWRAP(AT, a),				\
		       UNWRAP(BT, b),				\
		       UNWRAP(CT, c),				\
		       UNWRAP(DT, d),				\
		       UNWRAP(ET, e)));				\
  }

// TODO : (n-ary greater than 5 arguments need special treatment for byte-code)

// Helper macro to update values of type : ref int

#define Store_int_ref(vref, i) do { Field(vref, 0) = Val_int(i); } while (0)

/*
This should obviously only be used for values of type [int ref]!  This
works because ints are not (caml) heap pointers. Thus, we are not
changing anything of interest to the GC.  (if it doesn't, we only have
a single thing to/ fix!)
*/

#define FFI_COPY_LIST(N, RT, CF, AT)	\
  value N(value a, value vcount_r) {		\
    unsigned int count;				\
    RT list = CF(UNWRAP(AT, a), &count);	\
    Store_int_ref(vcount_r, count);		\
    return WRAP(heapptr, list);			\
  }

// ObjC external functions ___________________________________________________

FFI1( ObjC_malloc,
      heapptr, malloc, size_t )

FFI1( ObjC_free,
      void, free, voidptr )

FFI0( ObjC_make_null,
      pointer, NULL )

FFI1( ObjC_get_name,
      cstr, class_getName, Class)

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
      Ivar, class_getInstanceVariable, Class, cstr )

FFI2( Class_get_class_variable,
      Ivar, class_getClassVariable, Class, cstr )

FFI_COPY_LIST( Class_copy_ivar_list,
	       Ivar *, class_copyIvarList, Class )

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

FFI_COPY_LIST( Class_copy_method_list,
	       Method *, class_copyMethodList, Class )

FFI2( Class_conforms_to_protocol,
      BOOL, class_conformsToProtocol, Class, Protocol_ptr )

FFI1( Method_get_name,
      SEL, method_getName, Method )

FFI1( Method_get_implementation,
      IMP, method_getImplementation, Method )

FFI1( Method_get_type_encoding,
      cstr, method_getTypeEncoding, Method )

FFI1( Method_get_number_of_arguments,
      int, method_getNumberOfArguments, Method )

FFI1( Method_copy_return_type,
      heapptr, method_copyReturnType, Method )

FFI1( Method_copy_argument_type,
      heapptr, method_copyArgumentType, Method )


      

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

/*
static void	NSObject__finalize(value v);
//static int	NSObject__compare(value v1, value v2)
//static intnat	NSObject__hash(value v);
//static void	NSObject__serialize(value v, 
//				    uintnat *wsize_32,
//				    uintnat *wsize_64);
//static uintnat	NSObject__deserialize(void *dst);
//static int	NSObject__compare_ext(value v1, value v2);

static struct custom_operations NSObject__custom_ops = {
  .identifier	= "ch.lambdamuesli.ocaml-objc.NSObject",
  .finalize	= &NSObject__finalize,
  .compare	= custom_compare_default,
  .hash		= custom_hash_default,
  .serialize	= custom_serialize_default,
  .deserialize    = custom_deserialize_default,
  .compare_ext	= custom_compare_ext_default
};

#define NSObject__custom_val(v) ((NSObject **)Data_custom_val(v))

static void NSObject__finalize(value v) {
  // Do not use CAMLxyz macros in custom ops functions!
  NSObject **field = NSObject__custom_val(v);
  NSObject *object = *field;
  unsigned count = [object retainCount];
  if (count > 0) {
    NSLog(@"Object %@ has not been released: retain count still %d", object, count);
  }
}

static value NSObject__new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(v);
  v = caml_alloc_custom( &NSObject__custom_ops, sizeof(NSObject*), 0, 1);
  NSObject **field = NSObject__custom_val(v);
  *field = [NSObject new];
  CAMLreturn(v);
}

value NSObject_release(value v) {
  NSObject **field = NSObject__custom_val(v);
  NSObject *object = *field;
  [object release];
  field = NULL;
  return Val_unit;
}
*/
