include ZimtLang.DefModule (struct
  let name' = "ZimtCore"
  let requires = []
end)

open ZimtLang
open ZimtAST

let string = TPrim String
let unit = TPrim Unit
let stringL s = XLit (LitString s)



let printf = extern "printf" Fn.(arg string "format" ^^ varargs "args" ^^ ret unit)

let print_string = defun "print_string" Fn.(arg string "s" ^^ ret unit) (fun s ->
  printf (stringL "%s\n") (VPlus (s, VZero))
)
