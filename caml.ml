type _ t =
  | Unit	: unit t
  | Bool	: bool t
  | Float	: float t
  | Int		: int t
  | Int32	: int32 t
  | Int64	: int64 t
  | Nativeint	: nativeint t
  | Array	: 'a t		-> 'a array t
  | List	: 'a t		-> 'a list t
  | Option	: 'a t		-> 'a option t
  | Custom	: 'a custom	-> 'a custom t

and _ custom
