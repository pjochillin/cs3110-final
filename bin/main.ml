(** @author Joshua Ochalek (jo447), Krish Mehra (km937), Arnav Tevatia (at846) *)

open Yojson.Safe.Util
open Final

(** API Reference for Cohttp: https://github.com/mirage/ocaml-cohttp *)

let () = if Array.length Sys.argv < 2 then failwith "Must provide a ticker as an argument!"

let json = Api.time_series Sys.argv.(1)
(* let data = to_assoc (member "Time Series (5min)" json) *)

(* let rec make_table accx op hi lo cl i = function
| [] -> (Array.of_list (List.rev accx), Array.of_list op, Array.of_list hi, Array.of_list lo, Array.of_list cl)
| (_, v) :: t -> make_table (i :: accx) (float_of_string (to_string (member "1. open" v)) :: op) (float_of_string (to_string (member "2. high" v)) :: hi) (float_of_string (to_string (member "3. low" v)) :: lo) (float_of_string (to_string (member "4. close" v)) :: cl) (i +. 1.) t

let x, open_data, high_data, low_data, close_data = make_table [] [] [] [] [] 0. data *)

let () = Plot.make_plot (Api.range_x (List.length)) ()