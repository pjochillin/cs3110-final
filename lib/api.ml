open Cohttp_lwt_unix
open Lwt
open Yojson.Safe.Util

type t = (string * ((string * float) list)) list

let key = "RGWPOZSX9QV9O03C"

let time_series ticker =
  let body =
    Client.get (Uri.of_string ("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=" ^ String.uppercase_ascii ticker ^ "&interval=60min&apikey=" ^ key)) >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string (Lwt_main.run body)
  
let assoc_of_json json =
  let data = to_assoc (member "Time Series (60min)" json) in
  let extract_data el = 
    let time, info = el in
    let op = to_string (member "1. open" info) in
    let hi = to_string (member "2. high" info) in
    let lo = to_string (member "3. low" info) in
    let cl = to_string (member "4. close" info) in
    let vl = to_string (member "5. volume" info) in
    (time, [("1. open", float_of_string op); ("2. high", float_of_string hi); ("3. low", float_of_string lo); ("4. close", float_of_string cl); ("5. volume", float_of_string vl)]) in
  List.map extract_data data

let opens assoc =
  let map_fun el =
    snd (List.nth (snd el) 0) in
  Array.of_list (List.map map_fun assoc)

let highs assoc =
  let map_fun el =
    snd (List.nth (snd el) 1) in
  Array.of_list (List.map map_fun assoc)
  
let lows assoc =
  let map_fun el =
    snd (List.nth (snd el) 2) in
  Array.of_list (List.map map_fun assoc)

let closes assoc =
  let map_fun el =
    snd (List.nth (snd el) 3) in
  Array.of_list (List.map map_fun assoc)

let volumes assoc =
  let map_fun el =
    snd (List.nth (snd el) 4) in
  Array.of_list (List.map map_fun assoc)

let range_x n =
  let rec range_helper acc i = function                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
  | a when a = i -> List.rev acc
  | x -> range_helper (x :: acc) i (x + 1) in
  Array.of_list (List.map float_of_int (range_helper [] n 0))