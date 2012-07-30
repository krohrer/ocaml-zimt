#include <objc/objc.h>
#include <objc/objc-runtime.h>
#include "caml/mlvalues.h"

CAMLexport value objc_class_addIvar(value cls, value name, value size, value alignment, value types) {
  return cls;
}

