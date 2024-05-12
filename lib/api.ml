open Cohttp_lwt_unix
open Cohttp
open Lwt
open Yojson.Safe.Util

type t = ((string * float) list) list

let key = "RGWPOZSX9QV9O03C"
let polygon_key = "xua0vLksg2rJxtJNFwbq5UWBV54TybSB"
let rapidapi_key = "c923725323msh37a4a2d083c2e40p1a13e0jsn9fa9a4c4ccd3"

let time_series ticker =
  let body =
    Client.get (Uri.of_string ("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=" ^ 
                                String.uppercase_ascii ticker ^ "&interval=60min&apikey=" ^ key)) >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string (Lwt_main.run body)

let leap_year year =
  let one = year mod 4 = 0 && year mod 100 <> 0 in
  let two = year mod 400 = 0 in
  one || two

let rec days_before date = function
  | 0 -> date
  | x ->
    match date with
      | year, month, day -> 
        if day > 1 then 
          days_before (year, month, day - 1) (x - 1)
        else
          let days_in_month = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
          if month = 1 then
            days_before (year - 1, 12, 31) (x - 1)
          else
            let new_day = List.nth days_in_month (month - 2) in
            if new_day = 28 && leap_year year then
              days_before (year, month - 1, new_day + 1) (x - 1)
            else
              days_before (year, month - 1, new_day) (x - 1)

let polygon_series ticker =
  let api_domain = "https://api.polygon.io/" in
  (* Date retrieval is referenced from https://ocaml.org/cookbook/get-todays-date/stdlib *)
  let today = Unix.localtime (Unix.time ()) in
  let day = today.Unix.tm_mday in
  let month = today.Unix.tm_mon + 1 in
  let year = today.Unix.tm_year + 1900 in
  let to_date_string = Printf.sprintf "%04d-%02d-%02d" year month day in
  let old_year, old_month, old_day = days_before (year, month, day) 10 in
  let from_date_string = Printf.sprintf "%04d-%02d-%02d" old_year old_month old_day in
  let body =
    Client.get (Uri.of_string (api_domain ^ "v2/aggs/ticker/" ^ String.uppercase_ascii ticker ^ 
                                "/range/1/hour/" ^ from_date_string ^ "/" ^ to_date_string ^ 
                                "?adjusted=true&sort=asc&apiKey=" ^ polygon_key)) >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string (Lwt_main.run body)

let twelvedata_series ticker =
  let api_domain = "https://twelve-data1.p.rapidapi.com/" in
  (* API use is referenced from https://rapidapi.com/twelve-data1-twelve-data-default/api/twelve-data1/ *)
  let body =
    let uri = Uri.of_string (api_domain ^ "time_series?symbol=" ^ String.uppercase_ascii ticker ^ "&interval=1h&outputsize=100&format=json") in
    let headers = Header.add_list (Header.init ()) [
      ("X-RapidAPI-Key", rapidapi_key);
      ("X-RapidAPI-Host", "twelve-data1.p.rapidapi.com");
    ] in
    Client.call ~headers `GET uri
    >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string (Lwt_main.run body)

let apistocks_series ticker =
  let api_domain = "https://apistocks.p.rapidapi.com/" in
  (* Date retrieval is referenced from https://ocaml.org/cookbook/get-todays-date/stdlib *)
  let today = Unix.localtime (Unix.time ()) in
  let day = today.Unix.tm_mday in
  let month = today.Unix.tm_mon + 1 in
  let year = today.Unix.tm_year + 1900 in
  let to_date_string = Printf.sprintf "%04d-%02d-%02d" year month day in
  let old_year, old_month, old_day = days_before (year, month, day) 100 in
  let from_date_string = Printf.sprintf "%04d-%02d-%02d" old_year old_month old_day in
  (* API use is referenced from https://rapidapi.com/twelve-data1-twelve-data-default/api/twelve-data1/ *)
  let body =
    let uri = Uri.of_string (api_domain ^ "daily?symbol=" ^ String.uppercase_ascii ticker ^ "&dateStart=" ^
                             from_date_string ^ "&dateEnd=" ^ to_date_string) in
    let headers = Header.add_list (Header.init ()) [
      ("X-RapidAPI-Key", rapidapi_key);
      ("X-RapidAPI-Host", "apistocks.p.rapidapi.com");
    ] in
    Client.call ~headers `GET uri
    >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string (Lwt_main.run body)

let assoc_of_json json =
  let data = 
    if List.mem "Time Series (60min)" (keys json) then
      to_assoc (member "Time Series (60min)" json)
    else
      to_assoc (member "Time Series (Daily)" json) in
  let extract_data el = 
    let _, info = el in
    let op = to_string (member "1. open" info) in
    let hi = to_string (member "2. high" info) in
    let lo = to_string (member "3. low" info) in
    let cl = to_string (member "4. close" info) in
    let vl = to_string (member "5. volume" info) in
    [("1. open", float_of_string op); ("2. high", float_of_string hi); ("3. low", float_of_string lo); ("4. close", float_of_string cl); ("5. volume", float_of_string vl)] in
  List.map extract_data data

let assoc_of_polygon_json json =
  let data = member "results" json
    |> to_list in
  let extract_data el =
    let op = member "o" el
      |> to_number in
    let cl = member "c" el
      |> to_number in
    let hi = member "h" el
      |> to_number in
    let lo = member "l" el
      |> to_number in
    let vo = member "v" el
      |> to_number in
    [("1. open", op); ("2. high", hi); ("3. low", lo); ("4. close", cl); ("5. volume", vo)] in
  List.map extract_data data

let assoc_of_twelvedata_json json =
  let data = member "values" json
    |> to_list in
  let extract_data el =
    let op = member "open" el
      |> to_string
      |> float_of_string in
    let cl = member "close" el
      |> to_string
      |> float_of_string in
    let hi = member "high" el
      |> to_string
      |> float_of_string in
    let lo = member "low" el
      |> to_string
      |> float_of_string in
    let vo = member "volume" el
      |> to_string
      |> float_of_string in
    [("1. open", op); ("2. high", hi); ("3. low", lo); ("4. close", cl); ("5. volume", vo)] in
  List.map extract_data data

let assoc_of_apistocks_json json =
  let data = member "Results" json
    |> to_list in
  let extract_data el =
    let op = member "Open" el
      |> to_number in
    let cl = member "Close" el
      |> to_number in
    let hi = member "High" el
      |> to_number in
    let lo = member "Low" el
      |> to_number in
    let vo = member "Volume" el
      |> to_number in
    [("1. open", op); ("2. high", hi); ("3. low", lo); ("4. close", cl); ("5. volume", vo)] in
  List.map extract_data data

let opens assoc =
  let map_fun el =
    snd (List.nth el 0) in
  Array.of_list (List.map map_fun assoc)

let highs assoc =
  let map_fun el =
    snd (List.nth el 1) in
  Array.of_list (List.map map_fun assoc)
  
let lows assoc =
  let map_fun el =
    snd (List.nth el 2) in
  Array.of_list (List.map map_fun assoc)

let closes assoc =
  let map_fun el =
    snd (List.nth el 3) in
  Array.of_list (List.map map_fun assoc)

let volumes assoc =
  let map_fun el =
    snd (List.nth el 4) in
  Array.of_list (List.map map_fun assoc)

let range_x n =
  let rec range_helper acc i = function                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
  | a when a = i -> List.rev acc
  | x -> range_helper (x :: acc) i (x + 1) in
  Array.of_list (List.map float_of_int (range_helper [] n 0))