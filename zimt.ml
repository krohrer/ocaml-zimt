(** Type witnesses / phantom types for builtin types *)

(* C Language description hoisted into OCaml (with some new
constructs), using type witnesses and GADTs for added compile time
safety. (why write a typechecker when you can use OCaml's?) *)

(** Basic types *)
type void' = unit
type string' = string

(* Scalar types *)
type bool'	= [ `ZBool ]

type int'	= [ `ZInt ]
type int8'	= [ `ZInt8 ]
type int16'	= [ `ZInt16 ]
type int32'	= [ `ZInt32 ]
type int64'	= [ `ZInt64 ]
type intnat'	= [ `ZIntNat ]

type uint'	= [ `ZUInt ]
type uint8'	= [ `ZUInt8 ]
type uint16'	= [ `ZUInt16 ]
type uint32'	= [ `ZUInt32 ]
type uint64'	= [ `ZUInt64 ]
type uintnat'	= [ `ZUIntNat ]

type float32'	= [ `ZFloat32 ]
type float64'	= [ `ZFloat64 ]

(* Builtin type families *)
type sints'	= [  int' |  int8' |  int16' |  int32' |  int64' | intnat'  ]
type uints'	= [ uint' | uint8' | uint16' | uint32' | uint64' | uintnat' ]
type floats'	= [ float32' | float64' ]
type integers'	= [ sints' | uints' ]
type numbers'	= [ integers' | floats' ]
type scalar'	= [ numbers' | bool' ]

(** Identifiers *)
type ident = string
type filename = string
type header = [ `Sys of filename | `Usr of filename ] 

(** Environment *)
class type ['v,'t] environment =
object
  method env : ('v,'t) environment

  method requires : ('v,'t) environment list
  method includes : header list

  method lookup_value : ident -> 'v option
  method lookup_type : ident -> 't option
end

class type ['v,'t] mutable_environment =
object
  inherit ['v,'t] environment
  method add_include : header -> unit

  method add_value : ident -> 'v -> unit
  method add_type : ident -> 't -> unit
end

(** Definitions *)
type defvalue =
  | DefVar	: 'a x			-> defvalue
  | DefFunc	: 's fn * 'r x		-> defvalue
  | DefExt	: 's fn			-> defvalue

and deftype =
  | DefType	: 'a t			-> deftype

and env = (defvalue, deftype) environment
and mutenv = (defvalue, deftype) mutable_environment

and q_ident = env * ident

(** Type of expressions *)
and _ t =
  | TCaml	: 'a Caml.t	-> 'a Caml.t t
  | TForward	: 'a t Lazy.t	-> 'a t
  | TNamed	: 'a * q_ident	-> 'a t
  | TPtr	: 'a ptr	-> 'a ptr t
  | TStruct	: 'a struct'	-> 'a struct' t
  | TEnum	: 'a enum	-> 'a enum t
  | TPrim	: 'a prim	-> 'a t

and _ ptr =
  | PHeap	: 'a t		-> 'a ptr
  | PStatic	: 'a t		-> 'a ptr
  | PFn		: 's fn		-> 's fn ptr

and _ struct' =
  | SZero		: 'a					-> 'a struct'
  | SPlusField		: 'a struct' * ('b t*ident)		-> 'a struct'
  | SPlusBits		: 'a struct' * (int t*ident*int)	-> 'a struct'
  | SPlusPadding	: 'a struct' * (int t*int)		-> 'a struct'

and _ enum =
  | EZero	: 'a				-> 'a enum
  | EPlus	: 'a enum * (ident*int lit)	-> 'a enum

(* Arrays, pointers, structs *)
and (_,_) field =
  | FDeref	: 'a ptr		-> ('a ptr, 'a) field
  | FSubsript	: 'a ptr * int x	-> ('a ptr, 'a) field
  | FNamed	: 'a t * 'b t * ident	-> ('a, 'b) field

(* Primitive types *)
and _ prim =
  | Void	: void' prim
  | Bool	: bool' prim
  | String	: string' prim

  | Int		: int' prim
  | Int8	: int8' prim
  | Int16	: int16' prim
  | Int32	: int32' prim
  | Int64	: int64' prim
  | IntNat	: intnat' prim

  | UInt	: uint' prim
  | UInt8	: uint8' prim
  | UInt16	: uint16' prim
  | UInt32	: uint32' prim
  | UInt64	: uint64' prim
  | UIntNat	: uintnat' prim

  | Float32	: float32' prim
  | Float64	: float64' prim

(** Function signature *)
and _ fn =
  (*
    fn = arg* ret	: ('a x -> ... -> 'r x) fn
    fn = arg* varg ret	: ('a x -> ... -> varargs -> 'r x) fn
  *)

  (* Base case *)
  | FnRet	: 'r t				-> 'r x fn
  (* Variable arguments can only be at the last position *)
  | FnVarArgs	: ident * 'r x fn		-> (varargs->'r x) fn
  (* One dditional argument (TODO: can be unnamed) *)
  | FnArg	: 'a t * ident option * 'b fn	-> ('a x->'b) fn

(** Variadic function arguments *)
and varargs =
  | VZero	: 			   varargs
  | VPlus	: 'a x * varargs	-> varargs

(* Literals *)
and _ lit =
  | LitVoid	:		   void' lit
  | LitBool	: bool		-> bool' lit
  | LitString	: string	-> string' lit

  | LitInt8	: int		-> int8' lit
  | LitInt16	: int		-> int16' lit
  | LitInt32	: int32		-> int32' lit
  | LitInt64	: int64		-> int64' lit
  | LitIntNat	: nativeint	-> intnat' lit

  | LitUInt8	: int		-> int8' lit
  | LitUInt16	: int		-> int16' lit
  | LitUInt32	: int32		-> int32' lit
  | LitUInt64	: int64		-> int64' lit
  | LitUIntNat	: nativeint	-> uintnat' lit

  | LitInt	: int		-> int' lit
  | LitUInt	: int		-> uint' lit

(** Expressions *)
and _ x =
  (** Quotes *)
  | XQuote	: 'a t * string				-> 'a x
  (** Literals *)
  | XLit	: 'a lit				-> 'a x
  (** Identifiers *)
  | XId		: 'a t * q_ident			-> 'a x
  (** Function identifiers *)
  | XFnId	: 'a fn * q_ident			-> 'a fn x
  (** New bindings *)
  | XLet	: 'a t * q_ident * 'a x * ('a x->'b x)	-> 'b x
  (** Push one argument onto stack *)
  | XFnPushArg	: ('a x->'b) fn x * 'a x		-> 'b fn x
  (** Push variadic arguments onto stack, and call *)
  | XFnPushVA	: (varargs->'b x) fn x * varargs	-> 'b x fn x
  (** Call function with arguments on stack *)
  | XFnCall	: 'a x fn x				-> 'a x
  (** Unary operators *)
  | XOp1	: ('a,'b) op1 * 'a x			-> 'b x
  (** Binary operators *)
  | XOp2	: ('a,'b,'c) op2 * 'a x * 'b x		-> 'c x
  (** Conditional expression (why if if you can have cond?) *)
  | XCond	: (bool x*'a x) list * 'a x		-> 'a x
  (** Explicity sequencing of expressions *)
  | XDo		: void' x list * 'a x			-> 'a x
  (* TOOD : Add looping construct *)

(** Unary operators *)
and (_,_) op1 =
  | O1Arith	: 'a op1arith		-> ('a,'a) op1
  | O1Bit	: [`Not]		-> (int,int) op1
  | O1Logic	: [`Not]		-> (bool,bool) op1
  | O1SGet	: ('a,'b) field		-> ('a    ,'b) op1
  | O1FnDeref	:			   ('a fn ptr,'a fn) op1
  | O1FnRef	:			   ('a fn,'a fn ptr) op1

(** Binary operators *)
and (_,_,_) op2 =
  | O2Arith	: 'a op2arith	-> ('a,'a,'a) op2
  | O2PArith	: op2parith	-> ('a ptr,integers','a ptr) op2
  | O2Cmp	: op2cmp	-> (scalar',scalar',scalar') op2
  | O2Logic	: op2logic	-> (bool',bool',bool') op2
  | O2Bit	: op2bit	-> (integers',integers',integers') op2
  | O2SSet	: ('a,'b) field	-> ('a,'b,'b) op2

and op2parith	= [`Add|`Sub]
and op2cmp	= [`Eq|`NE|`Gt|`Lt|`GE|`LE]
and op2logic	= [`And|`Or]
and op2bit	= [`And|`Or|`Xor|`Shl|`Shr]

(** Unary arithmetic op *)
and _ op1arith =
  | A1Neg	: numbers' op1arith
  | A1PreInc	: integers' op1arith
  | A1PreDec	: integers' op1arith
  | A1PostInc	: integers' op1arith
  | A1PostDec	: integers' op1arith


(** Binary arithmetic op *)
and _ op2arith =
  | A2Add	: numbers' op2arith
  | A2Sub	: numbers' op2arith
  | A2Mul	: numbers' op2arith
  | A2Div	: numbers' op2arith
  | A2Mod	: integers' op2arith

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
    val type' : w t
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
    val ret : 'r t -> 'r x fn
    val varargs : ident -> 'r x fn -> (varargs->'r x) fn
    val arg : 'a t -> ident -> 'b fn -> ('a x->'b) fn
    val uarg : 'a t -> 'b fn -> ('a x->'b) fn

    (** Helpers *)
    val bind : env -> 's fn -> 's -> defvalue
    val apply : 's fn x -> 's
    val mkcall : 's fn -> 's fn x -> 's
  end

(* MODULE TYPE *)
module type MODULE =
  sig
    val name'		: ident
    val environment'	: mutenv

    (** Define composite types *)
    val enum'	: ident -> (module ENUM)
    val struct'	: ident -> (module STRUCT)

    (** EDSL for function signatures
	e.g. (arg int "a" ^^ arg bool "b" ^^ arg 
    *)
    module Fn : FN

    (** Define values *)
    val defvar'		: ident -> 'a t -> 'a x -> 'a x
    val defun'		: ident -> (_->_ as 's) fn -> 's -> 's * 's fn ptr x
    val extern'		: ident -> (_->_ as 's) fn -> 's * 's fn ptr x
  end
