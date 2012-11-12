open ObjC
;;

let classes = 
  let n = ref 0 in
  let pa = copy_class_list n in
  let a = Array.make !n "" in
  for i = 0 to !n - 1 do
    a.(i) <- C.copy_cstr (Class.get_name (C.ptr_array_ith pa i))
  done;
  C.free pa;
  a
;;

Array.iter print_endline classes
