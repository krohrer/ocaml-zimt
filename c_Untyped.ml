type rank = int
type associativity = [`L2R|`R2L]

module Type =
struct
  type t = C_UntypedAST.t
  type x = C_UntypedAST.x
  type q = C_UntypedAST.type_qual
  type s = C_UntypedAST.type_spec

  module B = Buffer
  open C_UntypedAST

  let precedence = function
    | TVoid
    | TBool
    | TInt _
    | TReal _
    | TRef _		-> 999999
    (* Pointer types have second highest precedence*)
    | TPointer _	-> 2
    (* Functions and array types have highest precedence *)
    | TFunc _		-> 1
    | TArray _		-> 1

  let associativity = function
    | r when r = 1	-> `L2R (* Function and array types bind left to right *)
    | r when r = 2	-> `R2L (* Pointers bind from right to left *)
    | r			-> `L2R (* The rest does not actually matter *)

  open Printf

  (* for lack of a better name we call it a fold *)


  let add_format b fmt = Printf.bprintf b fmt
  and add_string b s = Buffer.add_string b s

  let rec add_decl b (quals,spec) name =
    (* List.iter (add_type_qualifier b) quals; *)
    begin match spec with
    | TVoid			-> add_string b "void"
    (* | TChar ss 			-> add_int b ss "char" *)
    (* | TShort ss			-> add_int b ss "short" *)
    (* | TInt ss			-> add_int b ss "int" *)
    (* | TLong ss			-> add_int b ss "long" *)
    (* | TLongLong ss		-> add_int b ss "long long" *)
    (* | TFloat			-> add_string b "float" *)
    (* | TDouble			-> add_string b "double" *)
    (* | TLongDouble		-> add_string b "long double" *)
    (* | TBool			-> add_string b "bool" *)
    (* | TStructRef n		-> add_format b "struct %s" n *)
    (* | TUnionRef n		-> add_format b "union %s" n *)
    (* | TEnumRef n		-> add_format b "enum %s" n *)
    (* | TRef n			-> add_string b n *)
    | TPointer t		-> add_pointer b t name
    | TFunc (t,ats,va)		-> add_function b t ats va name
    | TArray (t,sizes)		-> add_array b t sizes name
    end

  and add_type_qualifier b = function
    | `const	-> add_string b "const " 
    | `restrict	-> add_string b "restrict "
    | `volatile	-> add_string b "volatile "

  and add_sign_spec b = function
    | `unsigned	-> add_string b "unsigned "
    | `signed	-> add_string b "signed "
    | `default	-> ()

  and add_int b ss name =
    add_sign_spec b ss;
    add_string b name

  and add_pointer b t name =
    ()

  and add_function b t ats va name =
    ()

  and add_array b t sizes name =
    ()

end

module Expr =
  struct
    type t = C_UntypedAST.x

    open C_UntypedAST
       
    (* http://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence *)

    (* Highest precedence is rank 0 for atomic expressions. Higher
    ranks mean lower precedence. *)

    let associativity = function
      | x when x < 3	-> `L2R
      | x when x < 4	-> `R2L
      | x when x < 15	-> `L2R
      | x when x < 17	-> `R2L
      | _		-> `L2R

    let rec precedence = function
      (* These are either atomic, or properly bracketed. *)
      | XIdent _
      | XStmtExpr _
      | XInit _
      | XDInit _			-> 0
      | XCall _				-> call_precedence
      | XLit l				-> lit_precedence l
      | XOp1 (o,_)			-> op1_precedence o
      | XOp2 (o,_,_)			-> op2_precedence o
      | XIIf (_,_,_)			-> 15
      (* We don't know what the quoted string contains, so we simply
      give it a very low precedence *)
      | XQuote s			-> 999999
    and call_precedence			=  2
    and lit_precedence = function
      (* Negative numbers have the same precedence as Op1Arith `Neg *)
      | LInt i	when i < 0		-> 3
      | LInt32 i	when i < 0l	-> 3
      | LInt64 i	when i < 0L	-> 3
      | LFloat32 f	when f < 0.0	-> 3
      | LFloat64 f	when f < 0.0	-> 3 
      (* The rest is atomic *)
      | _				-> 0
    and op1_precedence = function
      | Op1Arith `PostInc
      | Op1Arith `PostDec
      | Op1StructDeref _
      | Op1StructRef _			-> 2
      | Op1Arith `Neg
      | Op1Arith `PreInc
      | Op1Arith `PreDec
      | Op1Bit `Not
      | Op1Logic `Not
      | Op1Cast _
      | Op1Deref
      | Op1Ref				-> 3
    and op2_precedence = function
      | Op2Subscript			-> 2
      | Op2Arith `Mul
      | Op2Arith `Div
      | Op2Arith `Mod			-> 5
      | Op2Arith `Add
      | Op2Arith `Sub			-> 6
      | Op2Bit `ShiftL
      | Op2Bit `ShiftR			-> 7
      | Op2Comp `Gt
      | Op2Comp `Lt
      | Op2Comp `GE
      | Op2Comp `LE			-> 8
      | Op2Comp `Eq
      | Op2Comp `NE			-> 9
      | Op2Bit `And			-> 10
      | Op2Bit `Xor			-> 11
      | Op2Bit `Or			-> 12
      | Op2Logic `And			-> 13
      | Op2Logic `Or			-> 14
      | Op2Assign			-> 15
      | Op2Comma			-> comma_precedence
    and comma_precedence		=  17
end

module Stmt =
  struct
    type t = C_UntypedAST.st
end

module Ops =
struct
  include C_UntypedAST

  let var n		= XIdent n

  let call n args	= XCall (XIdent n, args)
  let apply x args	= XCall (x, args)

  let ( ~- ) x 		= XOp1 (Op1Arith `Neg, x)
  let inc x		= XOp1 (Op1Arith `PreInc, x)
  let dec x		= XOp1 (Op1Arith `PreDec, x)
  let postinc x		= XOp1 (Op1Arith `PostInc, x)
  let postdec x		= XOp1 (Op1Arith `PostDec, x)
  let not x		= XOp1 (Op1Logic `Not, x)
  let lnot x		= XOp1 (Op1Bit `Not, x)
  let cast t x		= XOp1 (Op1Cast t, x)
  let ( ^! ) x f	= XOp1 (Op1StructDeref f, x)
  let ( ^ ) x f		= XOp1 (Op1StructRef f, x)
  let ( ! ) x		= XOp1 (Op1Deref, x)
  let ref x		= XOp1 (Op1Ref, x)

  let ( := ) x y	= XOp2 (Op2Assign, x, y)
  let ( +! ) x y	= XOp2 (Op2Subscript, x, y)

  let ( + ) x y		= XOp2 (Op2Arith `Add, x, y)
  let ( - ) x y		= XOp2 (Op2Arith `Sub, x, y)
  let ( * ) x y		= XOp2 (Op2Arith `Mul, x, y)
  let ( / ) x y		= XOp2 (Op2Arith `Div, x, y)
  let ( mod ) x y	= XOp2 (Op2Arith `Mod, x, y)

  let (	= ) x y 	= XOp2 (Op2Comp `Eq, x, y)
  let ( == ) x y 	= XOp2 (Op2Comp `Eq, x, y)
  let ( <> ) x y	= XOp2 (Op2Comp `NE, x, y)
  let ( != ) x y	= XOp2 (Op2Comp `NE, x, y)
  let ( > ) x y		= XOp2 (Op2Comp `Gt, x, y)
  let ( < ) x y		= XOp2 (Op2Comp `Lt, x, y)
  let ( >= ) x y 	= XOp2 (Op2Comp `GE, x, y)
  let ( <= ) x y 	= XOp2 (Op2Comp `LE, x, y)

  let ( && ) x y	= XOp2 (Op2Logic `And, x, y)
  let ( || ) x y	= XOp2 (Op2Logic `Or, x, y)

  let ( land ) x y	= XOp2 (Op2Bit `And, x, y)
  let ( lor ) x y	= XOp2 (Op2Bit `Or, x, y)
  let ( lxor ) x y	= XOp2 (Op2Bit `Xor, x, y)
  let ( lsl ) x y	= XOp2 (Op2Bit `ShiftL, x, y)
  let ( lsr ) x y	= XOp2 (Op2Bit `ShiftR, x, y)
end
