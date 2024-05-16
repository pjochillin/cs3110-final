(** @author Joshua Ochalek (jo447), Krish Mehra (km937), Arnav Tevatia (at846) *)

open Final
open Bogue

let api_selected = ref "Alpha Vantage"

let main ticker =
  let series =
    if !api_selected = "Alpha Vantage" then Api.time_series ticker
    else if !api_selected = "Polygon.io" then Api.polygon_series ticker
    else if !api_selected = "Twelve Data" then Api.twelvedata_series ticker
    else Api.apistocks_series ticker
  in
  let json =
    if !api_selected = "Alpha Vantage" then Api.assoc_of_json series
    else if !api_selected = "Polygon.io" then Api.assoc_of_polygon_json series
    else if !api_selected = "Twelve Data" then
      Api.assoc_of_twelvedata_json series
    else Api.assoc_of_apistocks_json series
  in
  let open_data = Api.opens json in
  let high_data = Api.highs json in
  let low_data = Api.lows json in
  let close_data = Api.closes json in
  let volume_data = Api.volumes json in
  let x = Api.range_x (Array.length open_data) in
  let macd, signal = Analysis.macd (Array.to_list close_data) in
  let rsi = Analysis.rsi (Array.to_list close_data) 14 in
  let obv =
    Analysis.obv (Array.to_list volume_data) (Array.to_list close_data)
  in
  let atr =
    Analysis.atr (Array.to_list high_data) (Array.to_list low_data)
      (Array.to_list close_data)
  in
  let cci =
    Analysis.cci (Array.to_list high_data) (Array.to_list low_data)
      (Array.to_list close_data) 20
  in
  let bollinger_middle, bollinger_upper, bollinger_lower =
    Analysis.bollinger_bands (Array.to_list close_data) 20
  in
  let stochastic_k, stochastic_d =
    Analysis.stochastic_oscillator (Array.to_list high_data)
      (Array.to_list low_data) (Array.to_list close_data) 14 3
  in
  Plot.ticker_plot x open_data high_data low_data close_data;
  Plot.macd_plot (Array.of_list macd) (Array.of_list signal);
  Plot.rsi_plot (Array.of_list rsi);
  Plot.obv_plot (Array.of_list obv);
  Plot.atr_plot (Array.of_list atr);
  Plot.cci_plot (Array.of_list cci);
  Plot.bollinger_bands_plot
    (Array.of_list bollinger_middle)
    (Array.of_list bollinger_upper)
    (Array.of_list bollinger_lower);
  Plot.stochastic_plot (Array.of_list stochastic_k) (Array.of_list stochastic_d);
  Lwt.return_unit

let ticker_input = Widget.text_input ()

let main () =
  let top =
    [
      Widget.empty ~w:120 ~h:0 ();
      Widget.label "OCaml Financial Analysis" ?size:(Some 64)
        ?align:(Some Center);
    ]
    |> Layout.flat_of_w
  in
  let ticker_text = Widget.label "Enter a ticker below:" ?size:(Some 20) in
  let button_text =
    Label.create ?size:(Some 15) ?align:(Some Center) "Get Data"
  in
  let ticker_button = Widget.button ?label:(Some button_text) "Get Data" in
  let api_fun = function
    | 0 -> api_selected := "Alpha Vantage"
    | 1 -> api_selected := "Polygon.io"
    | 2 -> api_selected := "Twelve Data"
    | 3 -> api_selected := "APIStocks"
    | _ -> failwith "Chose unsupported option for dropdown!"
  in
  let api_options =
    Select.create ?action:(Some api_fun)
      [| "Alpha Vantage"; "Polygon.io"; "Twelve Data"; "APIStocks" |]
      0
  in
  let button_row =
    [ Layout.resident ticker_input; Layout.resident ticker_button; api_options ]
    |> Layout.flat ?align:(Some Center) ?hmargin:(Some 5)
  in
  let layout =
    [
      top;
      Layout.resident ticker_text;
      button_row;
      Layout.resident (Widget.empty ~w:1000 ~h:600 ());
    ]
    |> Layout.tower
  in
  let rec button_fun _ =
    try
      let () = Lwt_main.run (main (Widget.get_text ticker_input)) in
      let graph = Widget.image ~w:1000 ~h:580 "plot.jpeg" in
      let top =
        [
          Widget.empty ~w:120 ~h:0 ();
          Widget.label "OCaml Financial Analysis" ?size:(Some 64)
            ?align:(Some Center);
        ]
        |> Layout.flat_of_w
      in
      let button_text =
        Label.create ?size:(Some 15) ?align:(Some Center) "Get Data"
      in
      let ticker_button = Widget.button ?label:(Some button_text) "Get Data" in
      let api_fun = function
        | 0 -> api_selected := "Alpha Vantage"
        | 1 -> api_selected := "Polygon.io"
        | 2 -> api_selected := "Twelve Data"
        | 3 -> api_selected := "APIStocks"
        | _ -> failwith "Chose unsupported option for dropdown!"
      in
      let api_index =
        if !api_selected = "Alpha Vantage" then 0
        else if !api_selected = "Polygon.io" then 1
        else if !api_selected = "Twelve Data" then 2
        else 3
      in
      let api_options =
        Select.create ?action:(Some api_fun)
          [| "Alpha Vantage"; "Polygon.io"; "Twelve Data"; "APIStocks" |]
          api_index
      in
      let button_row =
        [
          Layout.resident ticker_input;
          Layout.resident ticker_button;
          api_options;
        ]
        |> Layout.flat ?align:(Some Center) ?hmargin:(Some 5)
      in
      let macd_graph = Widget.image ~w:350 ~h:200 "macd.jpeg" in
      let macd_tower =
        [ Widget.label "MACD"; macd_graph ] |> Layout.tower_of_w
      in
      let rsi_graph = Widget.image ~w:350 ~h:200 "rsi.jpeg" in
      let rsi_tower = [ Widget.label "RSI"; rsi_graph ] |> Layout.tower_of_w in
      let data_row1 = [ macd_tower; rsi_tower ] |> Layout.flat in
      let obv_graph = Widget.image ~w:350 ~h:200 "obv.jpeg" in
      let obv_tower = [ Widget.label "OBV"; obv_graph ] |> Layout.tower_of_w in
      let atr_graph = Widget.image ~w:350 ~h:200 "atr.jpeg" in
      let atr_tower = [ Widget.label "ATR"; atr_graph ] |> Layout.tower_of_w in
      let data_row2 = [ obv_tower; atr_tower ] |> Layout.flat in
      let data_tower = [ data_row1; data_row2 ] |> Layout.tower in
      let bollinger_graph = Widget.image ~w:350 ~h:200 "bollinger_bands.jpeg" in
      let bollinger_tower =
        [ Widget.label "Bollinger Bands"; bollinger_graph ] |> Layout.tower_of_w
      in
      let cci_graph = Widget.image ~w:350 ~h:200 "cci.jpeg" in
      let cci_tower = [ Widget.label "CCI"; cci_graph ] |> Layout.tower_of_w in
      let data2_row1 = [ bollinger_tower; cci_tower ] |> Layout.flat in
      let stochastic_graph = Widget.image ~w:350 ~h:200 "stochastic.jpeg" in
      let stochastic_tower =
        [ Widget.label "Stochastic Oscillator"; stochastic_graph ]
        |> Layout.tower_of_w
      in
      let articles =
        Lwt_main.run (Ticker_news.fetch_news (Widget.get_text ticker_input))
      in
      let to_text_display acc x =
        let chars_per_line = 78 in
        let rec to_display_helper x =
          if String.length x <= chars_per_line then x
          else
            let remaining_length = String.length x - chars_per_line in
            String.sub x 0 chars_per_line
            ^ "\n"
            ^ to_display_helper (String.sub x chars_per_line remaining_length)
        in
        acc ^ "\n" ^ to_display_helper x
      in
      let article_1 =
        Widget.text_display ~w:550 ~h:80
          (List.nth articles 0 |> List.map snd
          |> List.fold_left to_text_display ""
          |> String.trim)
      in
      let article_2 =
        Widget.text_display ~w:550 ~h:80
          (List.nth articles 1 |> List.map snd
          |> List.fold_left to_text_display ""
          |> String.trim)
      in
      let article_3 =
        Widget.text_display ~w:550 ~h:80
          (List.nth articles 2 |> List.map snd
          |> List.fold_left to_text_display ""
          |> String.trim)
      in
      let article_tower =
        [ article_1; article_2; article_3 ] |> Layout.tower_of_w
      in
      let data2_row2 = [ stochastic_tower; article_tower ] |> Layout.flat in
      let data2_tower = [ data2_row1; data2_row2 ] |> Layout.tower in
      let tabs =
        Tabs.create ~slide:Right
          [
            ("Graph", Layout.resident graph);
            ("MACD/RSI/OBV/ATR", data_tower);
            ("BOLL/CCI/STOCH/News", data2_tower);
          ]
      in
      let tower =
        [ top; Layout.resident ticker_text; button_row; tabs ] |> Layout.tower
      in
      let macd_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "macd.jpeg" in
        Layout.set_rooms data_tower [ Layout.resident overlay ];
        let macd_fun' _ =
          let old_tower = [ data_row1; data_row2 ] |> Layout.tower in
          Layout.set_rooms data_tower [ old_tower ]
        in
        Widget.on_click ~click:macd_fun' overlay
      in
      let rsi_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "rsi.jpeg" in
        Layout.set_rooms data_tower [ Layout.resident overlay ];
        let rsi_fun' _ =
          let old_tower = [ data_row1; data_row2 ] |> Layout.tower in
          Layout.set_rooms data_tower [ old_tower ]
        in
        Widget.on_click ~click:rsi_fun' overlay
      in
      let obv_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "obv.jpeg" in
        Layout.set_rooms data_tower [ Layout.resident overlay ];
        let obv_fun' _ =
          let old_tower = [ data_row1; data_row2 ] |> Layout.tower in
          Layout.set_rooms data_tower [ old_tower ]
        in
        Widget.on_click ~click:obv_fun' overlay
      in
      let atr_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "atr.jpeg" in
        Layout.set_rooms data_tower [ Layout.resident overlay ];
        let atr_fun' _ =
          let old_tower = [ data_row1; data_row2 ] |> Layout.tower in
          Layout.set_rooms data_tower [ old_tower ]
        in
        Widget.on_click ~click:atr_fun' overlay
      in
      let cci_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "cci.jpeg" in
        Layout.set_rooms data2_tower [ Layout.resident overlay ];
        let cci_fun' _ =
          let old_tower = [ data2_row1; data2_row2 ] |> Layout.tower in
          Layout.set_rooms data2_tower [ old_tower ]
        in
        Widget.on_click ~click:cci_fun' overlay
      in
      let bol_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "bollinger_bands.jpeg" in
        Layout.set_rooms data2_tower [ Layout.resident overlay ];
        let bol_fun' _ =
          let old_tower = [ data2_row1; data2_row2 ] |> Layout.tower in
          Layout.set_rooms data2_tower [ old_tower ]
        in
        Widget.on_click ~click:bol_fun' overlay
      in
      let stoch_fun _ =
        let overlay = Widget.image ~w:1000 ~h:550 "stochastic.jpeg" in
        Layout.set_rooms data2_tower [ Layout.resident overlay ];
        let stoch_fun' _ =
          let old_tower = [ data2_row1; data2_row2 ] |> Layout.tower in
          Layout.set_rooms data2_tower [ old_tower ]
        in
        Widget.on_click ~click:stoch_fun' overlay
      in
      Layout.set_rooms layout [ tower ];
      Widget.on_click ~click:button_fun ticker_button;
      Widget.on_click ~click:macd_fun macd_graph;
      Widget.on_click ~click:rsi_fun rsi_graph;
      Widget.on_click ~click:obv_fun obv_graph;
      Widget.on_click ~click:atr_fun atr_graph;
      Widget.on_click ~click:cci_fun cci_graph;
      Widget.on_click ~click:bol_fun bollinger_graph;
      Widget.on_click ~click:stoch_fun stochastic_graph
    with _ ->
      let top =
        [
          Widget.empty ~w:120 ~h:0 ();
          Widget.label "OCaml Financial Analysis" ?size:(Some 64)
            ?align:(Some Center);
        ]
        |> Layout.flat_of_w
      in
      let button_text =
        Label.create ?size:(Some 15) ?align:(Some Center) "Get Data"
      in
      let ticker_button = Widget.button ?label:(Some button_text) "Get Data" in
      let api_fun = function
        | 0 -> api_selected := "Alpha Vantage"
        | 1 -> api_selected := "Polygon.io"
        | 2 -> api_selected := "Twelve Data"
        | 3 -> api_selected := "APIStocks"
        | _ -> failwith "Chose unsupported option for dropdown!"
      in
      let api_index =
        if !api_selected = "Alpha Vantage" then 0
        else if !api_selected = "Polygon.io" then 1
        else if !api_selected = "Twelve Data" then 2
        else 3
      in
      let api_options =
        Select.create ?action:(Some api_fun)
          [| "Alpha Vantage"; "Polygon.io"; "Twelve Data"; "APIStocks" |]
          api_index
      in
      let button_row =
        [
          Layout.resident ticker_input;
          Layout.resident ticker_button;
          api_options;
        ]
        |> Layout.flat ?align:(Some Center) ?hmargin:(Some 5)
      in
      let error =
        Widget.label ?size:(Some 48) "Error: Invalid Ticker/API Out of Calls"
      in
      let tower =
        [
          top;
          Layout.resident ticker_text;
          button_row;
          Layout.resident error;
          Layout.resident (Widget.empty ~w:1000 ~h:600 ());
        ]
        |> Layout.tower
      in
      Layout.set_rooms layout [ tower ];
      Widget.on_click ~click:button_fun ticker_button
  in
  Widget.on_click ~click:button_fun ticker_button;
  layout |> Bogue.of_layout |> Bogue.run

let () = main ()
