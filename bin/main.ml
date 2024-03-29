open Lwt
open Cohttp_lwt_unix
open Yojson.Safe.Util

(** API Reference for Cohttp: https://github.com/mirage/ocaml-cohttp *)

let body =
  Client.get (Uri.of_string "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=IBM&interval=5min&apikey=demo") >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body

let json = Yojson.Safe.from_string (Lwt_main.run body)
let data = to_assoc (member "Time Series (5min)" json)

let rec make_table accx accy i = function
| [] -> (Array.of_list (List.rev accx), Array.of_list accy)
| (_, v) :: t -> make_table (i :: accx) (float_of_string (to_string (member "1. open" v)) :: accy) (i +. 1.) t

let x, y = make_table [] [] 0. data

let () = Final.Plot.make_plot x y