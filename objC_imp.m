#include "caml/mlvalues.h"
#include "caml/custom.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include <stdio.h>
#include <memory.h>


#include <objc/objc.h>
#include <objc/runtime.h>
// Do not include Foundation/Foundation.h directly -> type redefinition error: uint64.
// And I have not yet found a way around it.
#include <Foundation/NSObjCRuntime.h>
#include <Foundation/NSValue.h>


static void	NSObject__finalize(value v);
/*
static int	NSObject__compare(value v1, value v2)
static intnat	NSObject__hash(value v);
static void	NSObject__serialize(value v, 
				    uintnat *wsize_32,
				    uintnat *wsize_64);
static uintnat	NSObject__deserialize(void *dst);
static int	NSObject__compare_ext(value v1, value v2);
*/

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

/*
value caml_make_c_type(size_t size) {
  static value * closure_f = NULL;
  if (closure_f == NULL) {
    closure_f = caml_named_value("ml_c_type_make");
  }
  return caml_callback(*closure_f, Val_int(size));
}

#define DEF_C_TYPE(NAME, TYPE)			\
  CAMLexport value NAME##_make(value unit) {	\
    return caml_make_c_type(sizeof(TYPE));	\
  }


DEF_C_TYPE(c_char, char)
DEF_C_TYPE(c_ptr, void *)
DEF_C_TYPE(objc_class, Class)

CAMLexport value c_null_make(value size) {
  return (value) NULL;
}

CAMLexport value c_malloc(value size) {
  return (value) malloc(size);
}

CAMLexport value c_free(value ptr) {
  free((void *) ptr);
  return Val_unit;
}

CAMLexport value c_ptr_offset(value o, value ptr) {
  return (value) (((char *)ptr) + Int_val(o));
}

CAMLexport value c_ptr_to_string(value ptr) {
    CAMLlocal1(ml_data);
    char * data = (char *)ptr;
    int len = sizeof(data);
    ml_data = caml_alloc_string(len);
    memcpy(String_val(ml_data), data, len);
    return ml_data;
}

CAMLexport value c_string_to_string(value str) {
  return caml_copy_string((char *) str);
}

CAMLexport value objc_get_class_list(value ptr, value size) {
  return Val_int(objc_getClassList((Class *) ptr, Int_val(size)));
}
*/

CAMLexport value ObjC_get_class_list(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(ml_classes);

  int count = objc_getClassList(NULL, 0);
  Class *buffer = malloc(sizeof(Class)*count);

  ml_classes = caml_alloc(count, 0);
  
  objc_getClassList(buffer, count);
  for (int i = 0; i < count; ++i) {
    Store_field(ml_classes, i, (value) buffer[i]);
  }

  free(buffer);

  CAMLreturn(ml_classes);
}

CAMLexport value Class_get_name(value ptr) {
  return caml_copy_string(class_getName((Class) ptr));
}

/*
CAMLexport value objc_test(void) {
  int count = objc_getClassList(NULL, 0);
  Class *buffer = malloc(sizeof(Class)*count);

  objc_getClassList(buffer, count);

  NSLog(@"There are %d Objective-C classes.", count);
  for (int i = 0; i < count; ++i) {
    NSLog(@"Class : %s", class_getName(buffer[i]));
  }

  Class NSNumber = objc_getClass("NSNumber");
  id number = [NSNumber numberWithDouble:12345.6789];

  NSLog(@"NSNumber = %@", number);

  [number release];

  free(buffer);

  return Val_unit;
}
*/
