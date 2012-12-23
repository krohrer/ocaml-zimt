let is_empty = function
  | CEmpty	-> true
  | _		-> false
let is_not_empty c = not is_empty c

  

let rec simplify c = c
(*   | CSeq s		-> *)
(*     (match simplify_list s with *)
(*     | [] -> CEmpty *)
(*     | s	 -> CSeq s) *)
(*   | CBlock s		-> *)
(*     (match simplify_list s with *)
(*     | [] -> CEmpty *)
(*     | s	 -> CSeq s) *)
(*   | CSwitch (x,c)	-> CSwitch (x,simplify c) *)
(*   | CLabeled (l,c)	-> CLabeled *)
(*   | c			-> c *)
(* and simplify_list s = *)
(*   List.filter is_not_empty (List.map simplify s) *)
