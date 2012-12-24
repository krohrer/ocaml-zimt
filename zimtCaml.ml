open Printf

include (ZimtModule.Make(struct
  let name = "ZimtForeign"
  let includes =
    let caml n = `Usr (sprintf "caml/%s.h" n) in
    [
      caml "mlvalues";
      caml "custom";
      caml "alloc";
      caml "memory";
      caml "callback";
      caml "fail"
  ]
end))


