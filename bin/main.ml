open Lwt
open Cohttp_lwt_unix
open Yojson.Safe.Util

(** API Reference for Cohttp: https://github.com/mirage/ocaml-cohttp *)

let body =
  Client.get (Uri.of_string "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=IBM&interval=5min&apikey=demo") >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body

let json = Yojson.Safe.from_string (Lwt_main.run body)
let data = to_assoc (member "Time Series (5min)" json)

let rec make_table accx op hi lo cl i = function
| [] -> (Array.of_list (List.rev accx), Array.of_list op, Array.of_list hi, Array.of_list lo, Array.of_list cl)
| (_, v) :: t -> make_table (i :: accx) (float_of_string (to_string (member "1. open" v)) :: op) (float_of_string (to_string (member "2. high" v)) :: hi) (float_of_string (to_string (member "3. low" v)) :: lo) (float_of_string (to_string (member "4. close" v)) :: cl) (i +. 1.) t

let x, open_data, high_data, low_data, close_data = make_table [] [] [] [] [] 0. data

let () = Final.Plot.make_plot x open_data high_data low_data close_data