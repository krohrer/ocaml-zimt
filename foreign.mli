type type'
and name = string
and arg = type' * name

and cfdecl
and ccode = CEmpty | CStmt of cstmt | CSeq of ccode list | CBlock of ccode list
and cexpr = string
and cstmt = string

and entry

and cgenimp

module Type :
    sig
      type t = type'

      val make :
	  mlname:name ->
	    cname:name ->
	      cbox:(cexpr -> cexpr) ->
		cunbox:(cexpr -> cexpr) -> t

      val cbox		: t -> cexpr -> cexpr
      val cunbox	: t -> cexpr -> cexpr
      val cname		: t -> name
      val mlname	: t -> name
    end

module Entry :
    sig
      type t = entry

      val make :
	  mlname:name ->
	    rtype:type' ->
	      args:arg array ->
		cgenimp:cgenimp -> entry

      val mlname	: t -> name
      val rtype		: t -> type'
      val args		: t -> arg array
      val cgenimp	: t -> cgenimp
      val extname	: t -> name
      val extname_bc	: t -> name
      val numargs	: t -> int

      val argi : entry -> int -> arg
      val sig1 : entry -> type'*arg
      val sig2 : entry -> type'*arg*arg
      val sig3 : entry -> type'*arg*arg*arg
      val sig4 : entry -> type'*arg*arg*arg*arg
      val sig5 : entry -> type'*arg*arg*arg*arg*arg
      val sig6 : entry -> type'*arg*arg*arg*arg*arg*arg
      val sig7 : entry -> type'*arg*arg*arg*arg*arg*arg*arg
      val sig8 : entry -> type'*arg*arg*arg*arg*arg*arg*arg*arg
      val sig9 : entry -> type'*arg*arg*arg*arg*arg*arg*arg*arg*arg
      val sign : entry -> type'*arg array
    end

module CTypes :
    sig
      val value  : type'
      val void   : type'
      val int    : type'
      val string : type'
    end

module CCode :
    sig
      type t = ccode

      val stmtf : ('a, unit, string, t) format4 -> 'a

      val call : name -> cexpr array -> cexpr
      val return : cexpr -> t

      val box_return : type' -> cexpr -> t
      val unbox_arg : arg -> cexpr

      val let' : type' -> name -> cexpr -> t 
      val set : name -> cexpr -> t

      val letf' : type' -> name -> ('a, unit, string, t) format4 -> 'a
      val setf : name -> ('a, unit, string, t) format4 -> 'a

      val caml_params : name list -> t
      val caml_locals : name list -> t
      val caml_return : cexpr -> t

      val caml_safe_block :
	  ?params:name list ->
	    ?locals:name list ->
	      t -> cexpr -> t
    end

module Interface :
    sig
      val defconst : name -> type' -> cexpr -> entry
      val defun : name -> arg list -> type' -> cgenimp -> entry

      val ($) : ('a -> 'b) -> 'a -> 'b

      val dump : entry -> string
    end

