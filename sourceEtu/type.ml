type typ = Bool | Int | Rat | Undefined | Adr of typ | Null | Tid of string | Record of (typ * string) list
| RecordTds of typ list

let rec string_of_type t = 
  match t with
  | Bool ->  "Bool"
  | Int  ->  "Int"
  | Rat  ->  "Rat"
  | Undefined -> "Undefined"
  | Adr(typ) -> "Adress of " ^ string_of_type typ
  | Null -> "Null"
  | Tid(tid) -> tid
  


let rec est_compatible t1 t2 =
  match t1, t2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Rat, Rat -> true 
  | Adr _, Null -> true
  | Null, Adr _ -> true
  | Adr(typ1), Adr(typ2) -> est_compatible typ1 typ2 
  | Record(lp), Record(lp2) -> 
    let listtyp1 = List.map(fun (t,_)->t) lp in
    let listtyp2 = List.map(fun (t,_)->t) lp2 in
    est_compatible_list listtyp1 listtyp2
  | Record(lp), RecordTds(lp2) ->
    let listtyp1 = List.map(fun (t,_)->t) lp in
      est_compatible_list listtyp1 lp2
  | RecordTds(lp2), Record(lp) ->
    let listtyp1 = List.map(fun (t,_)->t) lp in
      est_compatible_list listtyp1 lp2
  | _ -> false 
and est_compatible_list lt1 lt2 =
  try
    List.for_all2 est_compatible lt1 lt2
  with Invalid_argument _ -> false

let%test _ = est_compatible Bool Bool
let%test _ = est_compatible Int Int
let%test _ = est_compatible Rat Rat
let%test _ = not (est_compatible Int Bool)
let%test _ = not (est_compatible Bool Int)
let%test _ = not (est_compatible Int Rat)
let%test _ = not (est_compatible Rat Int)
let%test _ = not (est_compatible Bool Rat)
let%test _ = not (est_compatible Rat Bool)
let%test _ = not (est_compatible Undefined Int)
let%test _ = not (est_compatible Int Undefined)
let%test _ = not (est_compatible Rat Undefined)
let%test _ = not (est_compatible Bool Undefined)
let%test _ = not (est_compatible Undefined Int)
let%test _ = not (est_compatible Undefined Rat)
let%test _ = not (est_compatible Undefined Bool)


let%test _ = est_compatible_list [] []
let%test _ = est_compatible_list [Int ; Rat] [Int ; Rat]
let%test _ = est_compatible_list [Bool ; Rat ; Bool] [Bool ; Rat ; Bool]
let%test _ = not (est_compatible_list [Int] [Int ; Rat])
let%test _ = not (est_compatible_list [Int] [Rat ; Int])
let%test _ = not (est_compatible_list [Int ; Rat] [Rat ; Int])
let%test _ = not (est_compatible_list [Bool ; Rat ; Bool] [Bool ; Rat ; Bool ; Int])

let rec getTaille t =
  match t with
  | Int -> 1
  | Bool -> 1
  | Rat -> 2
  | Undefined -> 0
  | Adr(typ) -> getTaille typ
  | Null -> 0
  
let%test _ = getTaille Int = 1
let%test _ = getTaille Bool = 1
let%test _ = getTaille Rat = 2
