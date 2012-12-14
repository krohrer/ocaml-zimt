#include "camlffi.h"

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

