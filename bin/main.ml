open Yojson.Safe.Util
open Final

let parse_json_to_tables json =
  let data = to_assoc (member "Time Series (60min)" json) in
  let rec make_table accx op hi lo cl i = function
    | [] ->
        ( Array.of_list (List.rev accx),
          Array.of_list op,
          Array.of_list hi,
          Array.of_list lo,
          Array.of_list cl )
    | (_, v) :: t ->
        make_table (i :: accx)
          (float_of_string (to_string (member "1. open" v)) :: op)
          (float_of_string (to_string (member "2. high" v)) :: hi)
          (float_of_string (to_string (member "3. low" v)) :: lo)
          (float_of_string (to_string (member "4. close" v)) :: cl)
          (i +. 1.) t
  in
  make_table [] [] [] [] [] 0. data

let main () =
  if Array.length Sys.argv < 2 then
    failwith "Must provide a ticker as an argument!";
  let ticker = Sys.argv.(1) in
  let series = Api.time_series ticker in
  let json = Api.assoc_of_json series in
  (* let x, open_data, high_data, low_data, close_data =
     parse_json_to_tables json *)
  let open_data = Api.opens json in
  let high_data = Api.highs json in
  let low_data = Api.lows json in
  let close_data = Api.closes json in
  let x = Api.range_x (Array.length open_data) in
  let rsi_values = Analysis.rsi (Array.to_list close_data) 14 in
  Printf.printf "[%a]\n"
    (fun ppf -> List.iter (Printf.fprintf ppf "%.2f; "))
    rsi_values;
  Final.Plot.make_plot x open_data high_data low_data close_data;
  Lwt.return_unit

let () = Lwt_main.run (main ())
