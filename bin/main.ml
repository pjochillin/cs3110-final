open Final

let main () =
   if Array.length Sys.argv < 2 then
     failwith "Must provide a ticker as an argument!";
   let ticker = Sys.argv.(1) in
   let series = Api.time_series ticker in
   let json = Api.assoc_of_json series in
   let open_data = Api.opens json in
   let high_data = Api.highs json in 
   let low_data = Api.lows json in
   let close_data = Api.closes json in
   let x = Api.range_x (Array.length open_data) in
   let rsi_values = Final.Analysis.rsi (Array.to_list close_data) 14 in
   Printf.printf "[%a]\n"
     (fun ppf -> List.iter (Printf.fprintf ppf "%.2f; "))
     rsi_values;
   let macd_line, _ = Final.Analysis.macd (Array.to_list close_data) in
   Printf.printf "[%a,]\n"
     (fun ppf -> List.iter (Printf.fprintf ppf "%.2f; "))
     macd_line;
   let _, signal_line = Final.Analysis.macd (Array.to_list close_data) in
   Printf.printf "[%a,]\n"
     (fun ppf -> List.iter (Printf.fprintf ppf "%.2f; "))
     signal_line;
   Plot.make_plot x open_data high_data low_data close_data;
   Lwt.return_unit

let () = Lwt_main.run (main ())
