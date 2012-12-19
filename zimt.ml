(** Type witnesses / phantom types for builtin types *)

(* C Language description hoisted into OCaml (with some new
constructs), using type witnesses and GADTs for added compile time
safety. (why write a typechecker when you can use OCaml's?) *)

(** Identifiers *)
type ident = string
type filename = string
type header =
  | IncludeSys of filename
  | IncludeUsr of filename

(** Environment *)
class type ['v,'t] environment =
object
  method env : ('v,'t) environment

  method requires : ('v,'t) environment list
  method includes : header list

  method lookup_value : ident -> 'v option
  method lookup_type : ident -> 't option

end

(** Definitions *)
type defvalue =
  | ValConst	: 'a lit		-> defvalue
  | ValFn	: ('r x,'s) fn * 'r x	-> defvalue
  | ValEx	: ('r,'s) fn		-> defvalue

and deftype =
  | Type	: 'a t			-> deftype

and env = (defvalue, deftype) environment

and q_ident = env * ident

(** Type of expressions *)
and 'a t =
  | TForward	: 'a t Lazy.t	-> 'a t
  | TNamed	: 'a * q_ident	-> 'a t
  | TPtr	: 'a ptr	-> 'a ptr t
  | TStruct	: 'a struct'	-> 'a struct' t
  | TEnum	: 'a enum	-> 'a enum t
  | TPrim	: 'a prim	-> 'a t
  | TFn		: ('r,'a) fn	-> ('r,'a) fn t
   
and 'a ptr =
  | PHeap		: 'a t	-> 'a ptr
  | PStatic		: 'a t	-> 'a ptr

and 'a struct' =
  | SZero		: 'a					-> 'a struct'
  | SPlusField		: 'a struct' * ('b t*ident)		-> 'a struct'
  | SPlusBits		: 'a struct' * (int t*ident*int)	-> 'a struct'
  | SPlusPadding	: 'a struct' * (int t*int)		-> 'a struct'

and 'a enum =
  | EZero	: 'a				-> 'a enum
  | EPlus	: 'a enum * (ident*int lit)	-> 'a enum

(* Arrays, pointers, structs *)
and ('a,'b) field =
  | FDeref	: 'a ptr		-> ('a ptr, 'a) field
  | FSubsript	: 'a ptr * int x	-> ('a ptr, 'a) field
  | FNamed	: 'a t * 'b t * ident	-> ('a, 'b) field

(* Primitive types *)
and _ prim =
  | Unit	: unit prim
  | Bool	: bool prim
  | Int		: int prim
  | Nativeint	: nativeint prim
  | Int32	: int32 prim
  | Int64	: int64 prim
  | Float	: float prim
  | String	: string prim

(** Function signature *)
and (_,_) fn =
  (* Base case *)
  | FLam0	: 'r t				-> ('r x,'r x) fn
  (* Variable arguments can only be at the last position *)
  | FLamV	: ident * ('r x,'r x) fn	-> ('r x,varargs->'r x) fn
  (* One dditional argument *)
  | FLam1	: 'a t * ident * ('r x,'b) fn	-> ('r x,'a x->'b) fn

(** Variadic function arguments *)
and varargs =
  | VZero	: varargs
  | VPlus	: 'a x * varargs	-> varargs

(* Literals *)
and _ lit = 
  | LitQuote	: 'a t * string	-> 'a lit
  | LitBool	: bool		-> bool lit
  | LitInt	: int		-> int lit
  | LitFloat	: float		-> float lit
  | LitString	: string	-> string lit

(** Expressions *)
and _ x =
  (** Literals *)
  | XLit	: 'a lit				-> 'a x
  (** Identifiers *)
  | XId		: 'a t * q_ident			-> 'a x
  (** New bindings *)
  | XLet	: 'a t * q_ident * 'a x * ('a x->'b x)	-> 'b x
  (** Function application, base case *)
  | XApp0	: ('r x,'r x) fn x			-> 'r x
  (** Function application, variadic *)
  | XAppV	: ('r x,varargs->'r x) fn x * varargs	-> ('r x,'r x) fn x
  (** Function application, recursive case *)
  | XApp1	: ('r x,'a x->'b) fn x * 'a x		-> ('r x,'b) fn x
  (** Unary operators *)
  | XOp1	: ('a,'b) op1 * 'a x			-> 'b x
  (** Binary operators *)
  | XOp2	: ('a,'b,'c) op2 * 'a x * 'b x		-> 'c x
  (** Conditional expression (why if if you can have cond?) *)
  | XCond	: (bool x*'a x) list * 'a x		-> 'a x
  (** Explicity sequencing of expressions *)
  | XDo		: unit x list * 'a x			-> 'a x
  (* TOOD : Add looping construct *)

(** Unary operators *)
and (_,_) op1 =
  | O1Arith	: 'a arith1		-> ('a,'a) op1
  | O1Bit	: [`Not]		-> (int,int) op1
  | O1Logic	: [`Not]		-> (bool,bool) op1
  | O1SGet	: ('a,'b) field		-> ('a    ,'b) op1

(** Binary operators *)
and (_,_,_) op2 =
  | O2Arith	: 'a arith2			-> ('a,'a,'a) op2
  | O2PArith	: [`Add|`Sub]			-> ('a ptr,int,'a ptr) op2
  | O2Comp	: [`Eq|`NE|`Gt|`Lt|`GE|`LE]	-> ('a,'a,'a) op2
  | O2Logic	: [`And|`Or]			-> (bool,bool,bool) op2
  | O2Bit	: [`And|`Or|`Xor|`Shl|`Shr]	-> (int,int,int) op2
  | O2SSet	: ('a,'b) field			-> ('a,'b,'b) op2


(** Unary arithmetic op *)
and _ arith1 =
  | A1Neg	: int arith1
  | A1PreInc	: int arith1
  | A1PreDec	: int arith1
  | A1PostInc	: int arith1
  | A1PostDec	: int arith1


(** Binary arithmetic op *)
and _ arith2 =
  | A2Add	: int arith2
  | A2Sub	: int arith2
  | A2Mul	: int arith2
  | A2Div	: int arith2
  | A2Mod	: int arith2

(* Mutable environments *)
class type mutenv =
object
  inherit [defvalue, deftype] environment

  method add_include : header -> unit

  method add_value : ident -> defvalue -> unit
  method add_type : ident -> deftype -> unit
end

(* NAMED MODULE *)
module type NAMED =
  sig
    val name' : string
  end

(* CONCRETE TYPE *)
module type TYPE =
  sig
    include NAMED

    type w
    val t' : w t
  end

(* Enumeration mixin, iterative, uses incomplete type for type witness
   [w] *)
module type ENUM =
  sig
    type e
    include TYPE with type w = e enum

    val case'	: ident -> int lit -> w x
  end

(** Structure mixin, see ENUM *)
module type STRUCT =
  sig
    type s
    include TYPE with type w = s struct'

    val field'	: 'a t -> ident -> (w,'a) field
    val bits'	: int t -> ident -> int -> (w,int) field
    val pad'	: int t -> int -> unit
  end

(* FUNCTION TYPE *)
module type FN =
  sig
    (** EDSL for function signatures

	e.g. (arg int "a" ^^ arg bool "b" ^^ arg unit) *)
    val (^^) : ('a -> 'b) -> 'a -> 'b
    val ret : 'r t -> ('r x,'r x) fn
    val varargs : ident -> ('r x,'r x) fn -> ('r x,varargs->'r x) fn
    val arg : 'a t -> ident -> ('r x,'b) fn -> ('r x,'a x->'b) fn

    (** Helpers *)
    val bind : ('r x,'s) fn -> 's -> 'r x
    val mkcall : ('r x,'s) fn -> ('r x,'s) fn x -> 's
  end

(* MODULE TYPE *)
module type MODULE =
  sig
    val name'		: ident
    val environment'	: mutenv

    (** Include header files *)
    val include' : header -> unit

    (** Define composite types *)
    val enum'	: ident -> (module ENUM)
    val struct'	: ident -> (module STRUCT)

    (** EDSL for function signatures
	e.g. (arg int "a" ^^ arg bool "b" ^^ arg 
    *)
    module Fn :
      sig
	val (^^) : ('a -> 'b) -> 'a -> 'b
	val ret : 'r t -> ('r x,'r x) fn
	val varargs : ident -> ('r x,'r x) fn -> ('r x,varargs->'r x) fn
	val arg : 'a t -> ident -> ('r x,'b) fn -> ('r x,'a x->'b) fn
      end

    (** Define values *)
    val defconst'	: ident -> 'a lit -> 'a x
    val defun'		: ident -> ('r x,'s) fn -> 's -> 's
    val extern'		: ident -> ('r x,'s) fn -> 's
  end
