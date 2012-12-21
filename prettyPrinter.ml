type t = Format.formatter -> unit

let pp_empty ff = ()
let (+++) f g = fun ff -> f ff; g ff
let ( *** ) f x = f x

(* f +++ pp_emtpy === pp_empty +++ f *)
(* (f +++ g) +++ h === f +++ (g +++ h) *)

let pp_string s ff = Format.pp_print_string ff s
let pp_int i ff = Format.pp_print_int ff i
let pp_spc ff = Format.pp_print_space ff ()

let pp_format fmt =
  Format.ksprintf (fun s ff -> Format.pp_print_string ff s) fmt

let pp_to_string f x ff = Format.pp_print_string ff (f x)
let pp_nbsp ff = Format.pp_print_string ff " "

let pp_list ~elem ?(sep=pp_empty) list ff =
  let rec fold = function
    | []	-> ()
    | [x]	-> elem x ff
    | x::rest	-> elem x ff; sep ff; fold rest
  in
  fold list

let pp_seq = List.fold_left (+++) pp_empty

let pp_bracket sopen sclose pp = pp_string sopen +++ pp +++ pp_string sclose
let pp_parenthesize pp = pp_bracket "(" ")" pp
let pp_bracket_curly pp = pp_bracket "{" "}" pp
let pp_bracket_square pp = pp_bracket "[" "]" pp

let pp_box ~ind pp ff =
  Format.pp_open_box ff ind;
  pp ff;
  Format.pp_close_box ff ()
  
let pp_hbox pp ff =
  Format.pp_open_hbox ff ();
  pp ff;
  Format.pp_close_box ff ()

let pp_vbox ~ind pp ff =
  Format.pp_open_vbox ff ind;
  pp ff;
  Format.pp_close_box ff ()

let pp_hvbox ~ind pp ff =
  Format.pp_open_hvbox ff ind;
  pp ff;
  Format.pp_close_box ff ()

let pp_cut ff = Format.pp_print_cut ff ()
let pp_brk nsp ind ff = Format.pp_print_break ff nsp ind

