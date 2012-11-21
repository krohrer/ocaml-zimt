open CTyped;;

let int8_t : int8' t = Obj.magic 0;;

let i = XLit (LInt8 0)
let x = XLet (int8_t, i, fun x ->
  let xv = XVar x in
  XSeq (XSet (xv, XLit (LInt8 0)),
	XOp2 (O2Arith A2Add, xv, xv)))

let f = XFLet (FLambda (int8_t,
			FVoid int8_t),
	       "blah",
	       fun x -> XVar x)

let _ =
  match x with
  | XLet (t, init, body) -> body (Obj.magic 0)
  | _ -> failwith "blah"


module T :
sig
  type readonly
  type readwrite = private readonly
  type immutable = private readonly
  type +'a t = private int

  val immutable_of_int : int -> immutable t
  val of_int : int -> readwrite t
  val print : readonly t -> unit

  val compare : 'a t -> 'b t -> int
end
=
struct
  type readonly
  type readwrite = private readonly
  type immutable = private readonly
  type 'a t = int

  let immutable_of_int i = i
  let of_int i = i

  let print i = Printf.printf "%d\n%!" i
  let compare i j = compare i j
end
