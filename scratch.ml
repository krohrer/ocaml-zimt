open Format;;

let _ =
  set_margin 20;;
  open_box 4;
  print_cut ();
  print_string "int test = abc + bcd;"; print_space ();
  print_break 0 ~-2; print_string "tis_but_a_label:"; print_space ();
  print_string "funcall(blah, bar, 3);"; print_space ();
  print_string "switch (really long expression)"
  close_box ()
