open Yojson.Safe.Util

type t = (string * ((string * string) list)) list

let assoc_of_json json =
  let data = to_assoc (member "Time Series (5min)" json) in
  let extract_data el = 
    let time, info = el in
    let op = member "1. open" info in
    let hi = member "2. high" info in
    let lo = member "3. low" info in
    let cl = member "4. close" info in
    let vl = member "5. volume" info in
    (time, [("1. open", op), ("2. high", hi), ("3. low", lo), ("4. close", cl), ("5. volume", vl)]) in
  List.map extract_data data

let opens assoc =
  let map_fun el =
    snd (List.nth (snd el) 0) in
  List.map map_fun assoc

let highs assoc =
  let map_fun el =
    snd (List.nth (snd el) 1) in
  List.map map_fun assoc
  
let lows assoc =
  let map_fun el =
    snd (List.nth (snd el) 2) in
  List.map map_fun assoc

let closes assoc =
  let map_fun el =
    snd (List.nth (snd el) 3) in
  List.map map_fun assoc

let volumes assoc =
  let map_fun el =
    snd (List.nth (snd el) 4) in
  List.map map_fun assoc

let rec range_x acc i = function                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
  | a when a = i -> List.rev acc
  | x -> range_x (x :: acc) i (x + 1)