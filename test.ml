open ObjC
;;

let classes = 
  let n = ref 0 in
  let pa = copy_class_list n in
  let a = Array.make !n C.null in
  for i = 0 to !n - 1 do
    a.(i) <- C.ptr_array_ith pa i
  done;
  C.free pa;
  a
;;

C.dump_cstr (Class.get_name classes.(0));;
