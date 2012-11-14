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
    e_mlname    : name;
    e_rtype	: type';
    e_cname	: name;
    e_args	: arg array;
    e_genimp	: genimp;
  }

and genimp = entry -> ccode

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

    let make ?mlname ~rtype ~cname ~args ~genimp =
      { e_mlname	= (match mlname with None -> cname | Some n -> n);
	e_rtype		= rtype;
	e_cname		= cname;
	e_args		= args;
	e_genimp	= genimp; }

    let mlname	{ e_mlname	= n; _ } = n
    let cname	{ e_cname	= n; _ } = n
    let rtype	{ e_rtype	= t; _ } = t
    let args	{ e_args	= a; _ } = a
    let genimp	{ e_genimp	= g; _ } = g

    let argi e i =
      let a = args e in
      let n = Array.length a in
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
    | args			-> sigfail 8 args
    and sign e = rtype e,args e
  end

(*--------------------------------------------------------------------------*)

module CCode =
  struct
    open Printf

    module T = Type

    type t = ccode

    let make_cfdecl name args =
      let argdecl (n, _) = "value "^n in
      sprintf "CAMLprim values caml_ffi_%s(%s)"
	name (String.concat ", " (List.map argdecl args))

    let stmtf fmt = ksprintf (fun s -> CStmt s) fmt
    let call n xs = sprintf "%s(%s)" n (String.concat ", " xs)
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

    let defimp e = failwith "Yeah... NO."
  end

let defconst ~mlname rtype expr =
  let cname	= mlname and
      args	= [|CTypes.void, "unit"|] and
      genimp _	= CCode.box_return rtype expr in
  Entry.make ~mlname ~rtype ~cname ~args ~genimp
    
let defun ?mlname rtype cname args genimp =
  Entry.make ?mlname ~rtype ~cname ~args:(Array.of_list args) ~genimp

let ($) f x = f x

let _ = [
  (* defun C.string "class_getName" [Class.t, "cls"] $ Imp.default; *)
  (* defun string "class_getName" [Class.t, "cls"] $ ... *)
]

(*--------------------------------------------------------------------------*)

module CDSL =
  struct
    open Printf
    module C = CCode
    module T = Type
    module E = Entry

    type c =
      | MZero
      | MOne of ccode
      | MAdd of c * c

    type 'a t = 'a * c

    let bind (a,cs) f = let b,cs' = f a in b, MAdd (cs, cs')
    and return x = x, MZero

    let rec eval f e = 
      let (), c = f e in
      CSeq (collect [] c)
    and collect accu = function
      | MZero -> accu
      | MOne c -> c :: accu
      | MAdd (c1, c2) -> collect (collect accu c2) c1

    let rtype e = return (E.rtype e)
    let cname e = return (E.cname e)
    let argi e i = return (E.argi e i)
    let sig1 e = return (E.sig1 e)
    let sig2 e = return (E.sig2 e)
    let sig3 e = return (E.sig3 e)
    let sig4 e = return (E.sig4 e)
    let sig5 e = return (E.sig5 e)
    let sig6 e = return (E.sig6 e)
    let sig7 e = return (E.sig7 e)
    let sig8 e = return (E.sig8 e)
    let sig9 e = return (E.sig9 e)
    let sign e = return (E.rtype e, E.args e)
    let ret x = (), MOne (C.return x)

    let stmtf fmt = ksprintf (fun s -> (), MOne (CStmt s)) fmt

    let declare t = stmtf "%s %s = %s" (T.cname t)
    let set = stmtf "%s = %s"

    let declaref t n fmt = ksprintf (stmtf "%s %s = %s" (T.cname t) n) fmt
    let setf n fmt = ksprintf (stmtf "%s = %s" n) fmt

    let call n xs = return (sprintf "%s(%s)" n (String.concat ", " xs))

    let unbox t x = return (T.cunbox t x)
    let unbox_arg (t,n) = unbox t n

    let box t x = return (T.cbox t x)
    let box_ret t x = (), MOne (C.box_return t x)
  end

