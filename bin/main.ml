open Bogue
open Cairo
module W = Widget
module L = Layout

let draw_chart cr data =
  let width = Cairo.Image.get_width (Cairo.get_target cr) in
  let height = Cairo.Image.get_height (Cairo.get_target cr) in
  let max_val = List.fold_left max min_float data in
  let min_val = List.fold_left min max_float data in

  Cairo.set_line_width cr 2.0;
  Cairo.set_source_rgb cr 0.1 0.5 0.8;

  List.iteri
    (fun i v ->
      let x = float_of_int i *. (float width /. float (List.length data - 1)) in
      let y =
        float_of_int height
        -. ((v -. min_val) /. (max_val -. min_val) *. float height)
      in
      if i = 0 then Cairo.move_to cr x y else Cairo.line_to cr x y)
    data;
  Cairo.stroke cr

let button_action canvas =
  let cr = Cairo.create canvas in
  draw_chart cr [ 10.; 20.; 30.; 25.; 15.; 18.; 22.; 19. ]

let setup_gui () =
  let top =
    [ W.label ~size:64 ~align:Center "OCaml Financial Analysis" ] |> L.flat_of_w
  in
  let ticker_text = W.label ~size:20 "Enter a ticker below:" in
  let ticker_input = W.text_input () in
  let button_text = Label.create ~size:15 ~align:Center "Get Data" in
  let canvas = W.empty 1000 600 () in
  let ticker_button = W.button ~label:button_text "Get Data" in
  let button_row = Layout.flat_of_w [ ticker_input; ticker_button ] in
  let canvas = W.empty 1000 600 () in
  let canvas_layout = Layout.resident canvas in
  [ top; Layout.resident ticker_text; button_row; canvas_layout ]
  |> Layout.tower |> Bogue.of_layout |> Bogue.run

let () = setup_gui ()

(* let main () =
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
   Lwt.return_unit *)
