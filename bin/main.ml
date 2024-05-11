open Final
open Bogue

let main ticker =
   (* if Array.length Sys.argv < 2 then
     failwith "Must provide a ticker as an argument!"; *)
   (* let ticker = Sys.argv.(1) in *)
   let series = Api.time_series ticker in
   let json = Api.assoc_of_json series in
   let open_data = Api.opens json in
   let high_data = Api.highs json in 
   let low_data = Api.lows json in
   let close_data = Api.closes json in
   let volume_data = Api.volumes json in
   let x = Api.range_x (Array.length open_data) in
   let macd, signal = Analysis.macd (Array.to_list close_data) in
   let rsi = Analysis.rsi (Array.to_list close_data) 14 in
   let obv = Analysis.obv (Array.to_list volume_data) (Array.to_list close_data) in
   (* let rsi_values = Final.Analysis.rsi (Array.to_list close_data) 14 in
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
     signal_line; *)
   Plot.ticker_plot x open_data high_data low_data close_data;
   Plot.macd_plot (Array.of_list macd) (Array.of_list signal);
   Plot.rsi_plot (Array.of_list rsi);
   Plot.obv_plot (Array.of_list obv);
   Lwt.return_unit

(* let () = Lwt_main.run (main ()) *)

let ticker_input = Widget.text_input ()

let () =
  let top = [Widget.empty 120 0 (); Widget.label "OCaml Financial Analysis" ?size:(Some 64) ?align:(Some Center)]
    |> Layout.flat_of_w in
  let ticker_text = Widget.label "Enter a ticker below:" ?size:(Some 20) in
  let button_text = Label.create ?size:(Some 15) ?align:(Some Center) "Get Data" in
  let ticker_button = Widget.button ?label:(Some button_text) "Get Data" in
  let button_row = [ticker_input; ticker_button]
    |> Layout.flat_of_w in
  let layout = [top; Layout.resident ticker_text; button_row; Layout.resident (Widget.empty 1000 600 ())]
    |> Layout.tower in
  let rec button_fun _ = 
    try
      let () = Lwt_main.run (main (Widget.get_text ticker_input)) in
      let graph = Widget.image ~w:1000 ~h:580 "plot.jpeg" in
      let top = [Widget.empty 120 0 (); Widget.label "OCaml Financial Analysis" ?size:(Some 64) ?align:(Some Center)]
        |> Layout.flat_of_w in
      let button_text = Label.create ?size:(Some 15) ?align:(Some Center) "Get Data" in
      let ticker_button = Widget.button ?label:(Some button_text) "Get Data" in
      let button_row = [ticker_input; ticker_button]
        |> Layout.flat_of_w in
      let ticker_graph = Widget.image ~w:400 ~h:250 "plot.jpeg" in
      let data_row1 = [ticker_graph; ticker_graph]
        |> Layout.flat_of_w in
      let macd_graph = Widget.image ~w:400 ~h:250 "macd.jpeg" in
      let rsi_graph = Widget.image ~w:400 ~h:250 "rsi.jpeg" in
      let data_row2 = [macd_graph; rsi_graph] 
        |> Layout.flat_of_w in 
      let data_tower = [data_row1; data_row2]
        |> Layout.tower in
      let obv_graph = Widget.image ~w:400 ~h:250 "obv.jpeg" in
      let data2_tower = [ticker_graph; obv_graph]
        |> Layout.tower_of_w in
      let tabs = Tabs.create ~slide:Right ["Graph", Layout.resident graph; "MACD/RSI", data_tower; "OBV", data2_tower] in
      let tower = [top; Layout.resident ticker_text; button_row; tabs]
        |> Layout.tower in
      let macd_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "macd.jpeg" in
        Layout.set_rooms data_tower [Layout.resident overlay];
        let macd_fun' _ =
          let old_tower = [data_row1; data_row2]
          |> Layout.tower in
          Layout.set_rooms data_tower [old_tower] in
        Widget.on_click macd_fun' overlay in
      let rsi_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "rsi.jpeg" in
        Layout.set_rooms data_tower [Layout.resident overlay];
        let rsi_fun' _ =
          let old_tower = [data_row1; data_row2]
          |> Layout.tower in
          Layout.set_rooms data_tower [old_tower] in
        Widget.on_click rsi_fun' overlay in
      let obv_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "obv.jpeg" in
        Layout.set_rooms data2_tower [Layout.resident overlay];
        let obv_fun' _ =
          let old_tower = [ticker_graph; obv_graph]
          |> Layout.tower_of_w in
          Layout.set_rooms data2_tower [old_tower] in
        Widget.on_click obv_fun' overlay in
      Layout.set_rooms layout [tower]; 
      Widget.on_click button_fun ticker_button;
      Widget.on_click macd_fun macd_graph;
      Widget.on_click rsi_fun rsi_graph;
      Widget.on_click obv_fun obv_graph
    with
      _ ->
        let top = [Widget.empty 120 0 (); Widget.label "OCaml Financial Analysis" ?size:(Some 64) ?align:(Some Center)]
          |> Layout.flat_of_w in
        let button_text = Label.create ?size:(Some 15) ?align:(Some Center) "Get Data" in
        let ticker_button = Widget.button ?label:(Some button_text) "Get Data" in
        let button_row = [ticker_input; ticker_button]
          |> Layout.flat_of_w in
        let tower = [top; Layout.resident ticker_text; button_row; Layout.resident (Widget.empty 1000 600 ())]
          |> Layout.tower in
        Layout.set_rooms layout [tower];
        Widget.on_click button_fun ticker_button in
  Widget.on_click button_fun ticker_button;
  layout
    |> Bogue.of_layout
    |> Bogue.run