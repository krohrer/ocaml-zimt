include ZimtModule.Make(struct
  let name = "ZimtCore"
  let includes = [
    `Sys "stdlib.h";
    `Sys "stdint.h";
    `Sys "opengl/opengl.h"
  ]
end)

open ZimtModule
open ZimtPrim
open Zimt

let string = TPrim ZString
let int = TPrim ZInt
let unit = TPrim ZUnit
let stringL s = XLit (LitString s)

let vararg = VZero
let vararg_plus x va = VPlus(x, va)

let (++) va x = vararg_plus x va

let printf = extern' "printf" Fn.(arg string "format" ^^ varargs "args" ^^ ret unit)

let print_string = defun' "print_string" Fn.(arg string "s" ^^ ret unit)
  (fun s ->
    printf (stringL "%s\n") (vararg ++ s))

module NSRange =
  struct
    include (val struct' "NSRange")
    let position	= field' int "position"
    let length		= field' int "length"
  end
