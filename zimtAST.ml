(** Type witnesses / phantom types for builtin types *)

(* C Language description hoisted into OCaml (with some new
constructs), using type witnesses and GADTs for added compile time
safety. (why write a typechecker when you can use OCaml's?) *)

(** Type of expressions *)
type 'a t =
  | TPtr	: 'a ptr -> 'a ptr t
  | TStruct	: 'a struct' -> 'a struct' t
  | TEnum	: 'a enum -> 'a enum t
  | TPrim	: 'a prim -> 'a t
  | TFun	: ('r,'a) fn -> ('r,'a) fn t
    
and 'a ptr =
  | PHeap		: 'a	-> 'a ptr
  | PStatic		: 'a	-> 'a ptr

and 'a struct' =
  | SZero		: 'a					-> 'a struct'
  | SPlusField		: 'a struct' * ('b t * ident)		-> 'a struct'
  | SPlusBits		: 'a struct' * (int t * ident * int)	-> 'a struct'
  | SPlusPadding	: 'a struct' * (int t * int)		-> 'a struct'

and 'a enum =
  | EZero	: 'a				-> 'a enum
  | EPlus	: 'a enum * (ident * int lit)	-> 'a enum

(* Arrays, pointers, structs *)
and ('a,'b) field =
  | FDeref	: 'a ptr -> ('a ptr, 'a) field
  | FSubsript	: 'a ptr * int x -> ('a ptr, 'a) field
  | FNamed	: 'a t * 'b t * ident -> ('a, 'b) field

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
  | FLam0	: 'r t				-> ('r x,'r x) fn
  | FLam1	: 'a t * ident * ('r x,'b) fn	-> ('r x,'a x -> 'b) fn

(* Literals *)
and _ lit = 
  | LitQuote	: 'a t * string	-> 'a lit
  | LitBool	: bool		-> bool lit
  | LitInt	: int		-> int lit
  | LitFloat	: float		-> float lit
  | LitString	: string	-> string lit

(** Identifiers *)
and ident = string

(** Expressions *)
and _ x =
  (** Literals *)
  | XLit	: 'a lit				-> 'a x
  (** Identifiers *)
  | XId		: 'a t * ident				-> 'a x
  (** New bindings *)
  | XLet	: 'a t * ident * 'a x * ('a x -> 'b x)	-> 'b x
  (** Function application, base case *)
  | XApp0	: ('r x,'r x) fn x			-> 'r x
  (** Function application, recursive case *)
  | XApp1	: ('r x,'a x -> 'b) fn x * 'a x		-> ('r x,'b) fn x
  (** Unary operators *)
  | XOp1	: ('a,'b) op1 * 'a x			-> 'b x
  (** Binary operators *)
  | XOp2	: ('a,'b,'c) op2 * 'a x * 'b x		-> 'c x
  (** Conditional expression (why if if you can have cond?) *)
  | XCond	: (bool x * 'a x) list * 'a x		-> 'a x
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
