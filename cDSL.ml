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

let call n xa =
  let xs = Array.to_list xa in
  let x = sprintf "%s(%s)" n (String.concat ", " xs) in
  return x

let unbox t x = return (T.cunbox t x)
let unbox_arg (t,n) = unbox t n

let box t x = return (T.cbox t x)
let box_ret t x = (), MOne (C.box_return t x)

