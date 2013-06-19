type _ t =
  | Unit	: unit t
  | Bool	: bool t
  | String	: string t
  | Float	: float t
  | Int		: int t
  | Int32	: int32 t
  | Int64	: int64 t
  | Nativeint	: nativeint t
  | Poly	: poly t
  | Array	: 'a t		-> 'a array t
  | List	: 'a t		-> 'a list t
  | Option	: 'a t		-> 'a option t
  | Custom	: 'a custom	-> 'a custom t
  | Tuple2	: 'a t * 'b t	-> ('a*'b) t
  | Tuple3	: 'a t * 'b t * 'c t -> ('a*'b*'c) t
  | Tuple4	: 'a t * 'b t * 'c t * 'd t -> ('a*'b*'c*'d) t

and 'a custom

and poly =
  | PNamed of string

type _ fn' =
  | LArg1Ret : 'a t * 'b t -> ('a->'b) fn'
  | LArg1 : 'a t * ('b->'r) fn' -> ('a->'b->'r) fn'
