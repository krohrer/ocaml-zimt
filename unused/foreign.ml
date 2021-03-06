type type' = {
    t_mlname	: name;
    t_cname	: name;
    t_cbox	: (cexpr -> cexpr);
    t_cunbox	: (cexpr -> cexpr);
  }
and name = string
and arg = type' * name

and cfdecl = string
and ccode = CEmpty | CStmt of cstmt | CSeq of ccode list | CBlock of ccode list
and cexpr = string
and cstmt = string

and entry = {
    e_mlname	: name;
    e_rtype	: type';
    e_args	: arg array;
    e_cgenimp	: cgenimp;
  }

and cgenimp = entry -> ccode

(*----------------------------------------------------------------------------*)

module Type =
  struct
    type t = type'

    let make ~mlname ~cname ~cbox ~cunbox =
      { t_mlname	= mlname;
	t_cname		= cname;
	t_cbox		= cbox;
	t_cunbox	= cunbox; }

    let cbox	{ t_cbox	= f; _ } = f
    let cunbox	{ t_cunbox	= f; _ } = f
    let cname	{ t_cname	= n; _ } = n
    let mlname	{ t_mlname	= n; _ } = n 
  end

(*--------------------------------------------------------------------------*)

module CTypes =
  struct
    open Printf

    let value =
      Type.make
	~mlname:"Obj.t"
	~cname:"value"
	~cbox:(fun x -> x)
	~cunbox:(fun x -> x)

    let void =
      Type.make
	~mlname:"unit"
	~cname:"void"
	~cbox:(sprintf "(%s, Val_unit)")
	~cunbox:(sprintf "(void)%s")

    let int =
      Type.make
	~mlname:"int"
	~cname:"int"
	~cbox:(sprintf "Val_int(%s)")
	~cunbox:(sprintf "Int_val(%s)")
	
    let string =
      Type.make
	~mlname:"string"
	~cname:"const char *"
	~cbox:(sprintf "caml_copy_string(%s)")
	~cunbox:(sprintf "String_val(%s)")
  end
    
(*--------------------------------------------------------------------------*)

module Entry =
  struct
    open Printf

    type t = entry

    let make ~mlname ~rtype ~args ~cgenimp =
      { e_mlname	= mlname;
	e_rtype		= rtype;
	e_args		= args;
	e_cgenimp	= cgenimp }

    let mlname	{ e_mlname	= n; _ } = n
    let rtype	{ e_rtype	= t; _ } = t
    let args	{ e_args	= a; _ } = a
    let cgenimp	{ e_cgenimp	= g; _ } = g

    let extname e	= sprintf "camlffi_%s" (mlname e)
    let extname_bc e	= sprintf "camlffi_%s__bc" (mlname e)

    let numargs e = Array.length (args e)

    let needs_bytecode e = numargs e > 5

    let argi e i =
      let a = args e in
      let n = numargs e in
      if i < n then
	a.(i)
      else
	failwith (sprintf "Entry.argi _ %d: Only %d arguments." i n)

    let sigfail n args =
      failwith (sprintf "Entry.sig%d _: Only %d arguments."
		  n (Array.length args))

    let sig1 y = match args y with
      [|a|]			-> rtype y,a
    | args			-> sigfail 1 args
    and sig2 y = match args y with
      [|a;b|]			-> rtype y,a,b
    | args			-> sigfail 2 args
    and sig3 y = match args y with
      [|a;b;c|]			-> rtype y,a,b,c
    | args			-> sigfail 3 args
    and sig4 y = match args y with
      [|a;b;c;d|]		-> rtype y,a,b,c,d
    | args			-> sigfail 4 args
    and sig5 y = match args y with
      [|a;b;c;d;e|]		-> rtype y,a,b,c,d,e
    | args			-> sigfail 5 args
    and sig6 y = match args y with
      [|a;b;c;d;e;f|]		-> rtype y,a,b,c,d,e,f
    | args			-> sigfail 6 args
    and sig7 y = match args y with
      [|a;b;c;d;e;f;g|]		-> rtype y,a,b,c,d,e,f,g
    | args			-> sigfail 7 args
    and sig8 y = match args y with
      [|a;b;c;d;e;f;g;h|]	-> rtype y,a,b,c,d,e,f,g,h
    | args			-> sigfail 8 args
    and sig9 y = match args y with
      [|a;b;c;d;e;f;g;h;i|]	-> rtype y,a,b,c,d,e,f,g,h,i
    | args			-> sigfail 9 args
    and sign e = rtype e,args e
  end

(*--------------------------------------------------------------------------*)

module CCode =
  struct
    open Printf

    module T = Type
    module E = Entry

    type t = ccode

    let stmtf fmt = ksprintf (fun s -> CStmt s) fmt
    let call n xs = sprintf "%s(%s)" n (String.concat ", " (Array.to_list xs))
    let return x = CStmt (sprintf "return %s" x)
    let box_return t x = return (T.cbox t x)
    let unbox_arg (t,n) = T.cunbox t n

    let let' t n x = stmtf "%s %s = %s" (T.cname t) n x
    let letf' t n fmt = ksprintf (stmtf "%s %s = %s" (T.cname t) n) fmt

    let set n x = stmtf "%s = %s" n x
    let setf n fmt = ksprintf (stmtf "%s = %s" n) fmt

    let caml_macro ?init name args =
      let mkstmt args =
	stmtf "%s%d(%s)" name (List.length args) (String.concat ", " args)
      in
      let rec loop raccu = function
	  (* No more than 5 elements per macro *)
	| []			-> raccu
	| (a::b::c::d::e::rest) -> loop (mkstmt [a;b;c;d;e] :: raccu) rest
	| args			-> mkstmt args :: raccu
      in
      let init' = match init with None -> [] | Some s -> [CStmt s] in
      let stmts = List.rev (loop init' args) in
      CSeq stmts

    let caml_params ps = caml_macro ~init:"CAMLparam0()" "CAMLxparam" ps
    let caml_locals ls = caml_macro "CAMLlocal" ls
    let caml_return x = stmtf "CAMLreturn(%s)" x

    let caml_safe_block ?(params=[]) ?(locals=[]) code x =
      match params, locals with
      | [], [] ->
	  CBlock [ code;
		   return x ]
      | ps, ls ->
	  CBlock [ caml_params ps;
		   caml_locals ls;
		   code;
		   caml_return x ]

    let wrap_cfun e =
      let rtype = E.rtype e and
	  args	= E.args e and
	  cname = E.mlname e in
      let unboxed_args = Array.map unbox_arg args in
      CSeq [ let' rtype "r" (call cname unboxed_args);
	     box_return rtype "r" ]
	     
  end

(*--------------------------------------------------------------------------*)

module Interface =
  struct
    module E = Entry
    module T = Type
    module C = CCode
    module B = Buffer
    open Printf

    let defconst mlname rtype expr =
      let args		= [|CTypes.void, "unit"|] and
	  cgenimp _	= CBlock [ C.box_return rtype expr ] in
      Entry.make ~mlname ~rtype ~args ~cgenimp
	
    let defun mlname args rtype cgenimp =
      Entry.make ~mlname ~rtype ~args:(Array.of_list args) ~cgenimp

    let ($) f x = f x

    module F = Format

    let pps f s =
      F.pp_print_string f s

    let ppf f fmt =
      F.fprintf f fmt

    let rec pp_ccode f ?(needs_block=true) = function
	| CBlock cs		-> pp_block f cs
	| c when needs_block	-> pp_block f [c]
	| CSeq cs		-> pp_seq f cs
	| CStmt s		-> pp_stmt f s
	| CEmpty		-> ppf f "@,"
    and pp_block f cs =
      ppf f "@[<v 2>{ ";
      pp_seq f cs;
      ppf f "@]@ }"
    and pp_seq f cs =
      List.iter (pp_ccode f ~needs_block:false) cs
    and pp_stmt f s =
      ppf f "@ %s;" s

    let pp_ml_signature f e =
      for i = 0 to E.numargs e - 1 do
	let t,_ = E.argi e i in
	ppf f "%s ->@ " (T.mlname t);
      done;
      pps f (T.mlname (E.rtype e)) 

    let pp_ml_fdecl f e =
      let mlname = E.mlname e in
      ppf f "@[<v 2>@[<h>external %s :@ " mlname;
      pp_ml_signature f e;
      ppf f " =@]@,";
      if E.needs_bytecode e then
	ppf f "%S@ " (E.extname_bc e);
      ppf f "%S@]" (E.extname e)
  
    let pp_c_fdecl f ?(term=";") e =
      ppf f "CAMLprim value %s (@[<hv>" (E.extname e);
      for i = 0 to E.numargs e - 1 do
	let _,an = E.argi e i in
	if i > 0 then ppf f ",@ ";
	ppf f "value %s" an;
      done;
      ppf f "@])%s" term

    let pp_c_fdef f e =
      ppf f "@[<v 0>";
      pp_c_fdecl f ~term:"" e;
      ppf f "@ ";
      pp_ccode f (E.cgenimp e e);
      ppf f "@]"
      
    let pp_c_fdecl_bytecode f ?(term=";") e =
      ppf f "CAMLprim value %s (value * argv, int argn)%s" (E.extname_bc e) term

    let pp_c_fbody_bytecode f e =
      ppf f "@[<v 2>{@ return %s (@[<hv>" (E.extname e);
      for i = 0 to E.numargs e - 1 do
	if i > 0 then ppf f ",@ ";
	ppf f "argv[%d]" i
      done;
      ppf f "@]);@]@ }"

    let pp_c_fdef_bytecode f e =
      ppf f "@[<v 0>";
      pp_c_fdecl_bytecode f ~term:"" e;
      ppf f "@ ";
      pp_c_fbody_bytecode f e;
      ppf f "@]"

    let rec dump e =
      let b = B.create 1024 in
      let f = F.formatter_of_buffer b in
      F.pp_open_vbox f 0;
      pp_ml_fdecl f e;
      F.pp_print_cut f ();
      pp_c_fdecl_bytecode f e;
      F.pp_print_cut f ();
      pp_c_fdecl f e;
      F.pp_print_cut f ();
      pp_c_fdef_bytecode f e;
      F.pp_print_cut f ();
      pp_c_fdef f e;
      F.pp_close_box f ();
      F.pp_print_flush f ();
      B.contents b
  end

(*--------------------------------------------------------------------------*)

open Interface
open CTypes
open CCode

let _ =
  let entries = [
    defun "class_getName"
      [int, "cls"; string, "str"] string 
      $ wrap_cfun;
    defun "class_warfare"
      [int, "a"; int, "b"; int, "c"; int, "d"; int, "e"; int, "f"] string
      $ wrap_cfun;
  ] in
  List.iter (fun x -> print_endline (dump x)) entries

