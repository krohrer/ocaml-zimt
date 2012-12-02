type x

type t = type_qual list * type_spec
and type_spec = 
  | TVoid
  | TChar of sign_spec
  | TShort of sign_spec
  | TInt of sign_spec
  | TLong of sign_spec
  | TLongLong of sign_spec
  | TFloat
  | TDouble
  | TLongDouble
  | TBool
  | TStructRef of ident
  | TUnionRef of ident
  | TEnumRef of ident
  | TRef of ident
  | TPointer of t
  | TFPointer of t * t list * varargs
  | TArray of t * array_sizes
and varargs = bool
and type_qual = [`const | `restrict | `volatile]
and ident = string
and sign_spec = [`unsigned | `signed | `default]
and field_decl = 
  | FField of t * ident
  | FBitField of t * ident * int
  | FBitPadding of type_spec * int
and enumerator = ident * x option
and declaration = storage_class * t
and storage_class = [`extern | `static | `auto | `register]
and array_sizes = int array


open Format

(* TODO : Use *)
let rec print_type_spec ff name = function
  | TVoid		-> pp_print_string ff "void"
  | TChar ss		-> print_with_sign_spec ff ss "char"
  | TShort ss		-> print_with_sign_spec ff ss "short"
  | TInt ss		-> print_with_sign_spec ff ss "int"
  | TLong ss		-> print_with_sign_spec ff ss "long"
  | TLongLong ss	-> print_with_sign_spec ff ss "long long"
  | TFloat		-> pp_print_string ff "float"
  | TDouble		-> pp_print_string ff "double"
  | TLongDouble		-> pp_print_string ff "long double"
  | TBool		-> pp_print_string ff "bool"
  | TStructRef name	-> fprintf ff "struct %s" name
  | TUnionRef name	-> fprintf ff "union %s" name
  | TEnumRef nam	-> fprintf ff "enum %s" name
  | TRef name		-> pp_print_string ff name
  (* These three are special, because they are recursive, and because
     C has a really strange syntax for types, especially for these
     three *)
  | TPointer t		-> print_pointer ff t name
  | TFunc (t,targs,va)	-> print_fun ff t name
  | TArray (t,sizes)	-> print_array ff t name


and print_with_sign_spec ff ss tname name =
  match ss with 
  | `unsigned -> fprintf ff "unsigned %s@ " name
  | `signed -> fprintf ff "signed %s@ " name
  | `default -> pp_print_string ff name


and print_pointer ff name name (tq, ts) =
  match ts with
  | TPointer _ as tp -> ()
  | TFun _ as tf -> ()
  | TArray _ as ta -> ()

and print_fun ff name t targs vararg =

and print_array ff name t sizes =
