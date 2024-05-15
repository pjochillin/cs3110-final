open Plplot

let ticker_plot x open_price high_price low_price close_price =
  let y_min =
    Array.fold_left
      (fun curr_min el -> min curr_min el)
      high_price.(0) high_price
  in
  let y_max =
    Array.fold_left
      (fun curr_min el -> max curr_min el)
      high_price.(0) high_price
  in
  plsfnam "plot.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2;
  plenv 0.0 (float_of_int (Array.length x + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 3;
  Array.iteri
    (fun i _ ->
      let i_f = float_of_int i in
      plline [| i_f; i_f |] [| low_price.(i); high_price.(i) |];

      plline [| i_f -. 0.2; i_f +. 0.2 |] [| open_price.(i); open_price.(i) |];
      plline [| i_f -. 0.2; i_f +. 0.2 |] [| close_price.(i); close_price.(i) |];

      if close_price.(i) >= open_price.(i) then
        plfill
          [| i_f -. 0.2; i_f +. 0.2; i_f +. 0.2; i_f -. 0.2 |]
          [| open_price.(i); open_price.(i); close_price.(i); close_price.(i) |]
      else
        plfill
          [| i_f -. 0.2; i_f +. 0.2; i_f +. 0.2; i_f -. 0.2 |]
          [| close_price.(i); close_price.(i); open_price.(i); open_price.(i) |])
    x;
  plend ()

let macd_plot macd_line signal_line =
  let temp_min =
    Array.fold_left (fun curr_min el -> min curr_min el) macd_line.(0) macd_line
  in
  let y_min =
    Array.fold_left (fun curr_min el -> min curr_min el) temp_min signal_line
  in
  let temp_max =
    Array.fold_left (fun curr_min el -> max curr_min el) macd_line.(0) macd_line
  in
  let y_max =
    Array.fold_left (fun curr_min el -> max curr_min el) temp_max signal_line
  in
  let x = Api.range_x (Array.length macd_line) in
  plsfnam "macd.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2;
  plwidth 2.0;
  plenv 0.0 (float_of_int (Array.length x + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 3;
  plline x macd_line;
  plcol0 5;
  plline x signal_line;
  plend ()

let rsi_plot rsi_line =
  let y_min =
    Array.fold_left (fun curr_min el -> min curr_min el) rsi_line.(0) rsi_line
  in
  let y_max =
    Array.fold_left (fun curr_min el -> max curr_min el) rsi_line.(0) rsi_line
  in
  let x = Api.range_x (Array.length rsi_line) in
  plsfnam "rsi.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2;
  plwidth 2.0;
  plenv 0.0 (float_of_int (Array.length x + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 3;
  plline x rsi_line;
  plend ()

let obv_plot obv_line =
  let y_min =
    Array.fold_left (fun curr_min el -> min curr_min el) obv_line.(0) obv_line
  in
  let y_max =
    Array.fold_left (fun curr_min el -> max curr_min el) obv_line.(0) obv_line
  in
  let x = Api.range_x (Array.length obv_line) in
  plsfnam "obv.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2;
  plwidth 2.0;
  plenv 0.0 (float_of_int (Array.length x + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 9;
  plline x obv_line;
  plend ()

let atr_plot atr_line =
  let y_min =
    Array.fold_left (fun curr_min el -> min curr_min el) atr_line.(0) atr_line
  in
  let y_max =
    Array.fold_left (fun curr_min el -> max curr_min el) atr_line.(0) atr_line
  in
  let x = Api.range_x (Array.length atr_line) in
  plsfnam "atr.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2;
  plwidth 2.0;
  plenv 0.0 (float_of_int (Array.length x + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 15;
  plline x atr_line;
  plend ()

let cci_plot cci_line =
  let y_min =
    Array.fold_left (fun curr_min el -> min curr_min el) cci_line.(0) cci_line
  in
  let y_max =
    Array.fold_left (fun curr_min el -> max curr_min el) cci_line.(0) cci_line
  in
  let x = Api.range_x (Array.length cci_line) in
  plsfnam "cci.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2;
  plwidth 2.0;
  plenv 0.0 (float_of_int (Array.length x + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 15;
  plline x cci_line;
  plend ()

let bollinger_bands_plot top_band middle_band bottom_band =
  let temp_min =
    Array.fold_left
      (fun curr_min el -> min curr_min el)
      top_band.(0) bottom_band
  in
  let y_min =
    Array.fold_left (fun curr_min el -> min curr_min el) temp_min middle_band
  in
  let temp_max =
    Array.fold_left (fun curr_max el -> max curr_max el) top_band.(0) top_band
  in
  let y_max =
    Array.fold_left (fun curr_max el -> max curr_max el) temp_max bottom_band
  in
  let x = Api.range_x (Array.length top_band) in
  plsfnam "bollinger_bands.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2;
  plwidth 2.0;
  plenv 0.0 (float_of_int (Array.length x + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 3;
  plline x top_band;
  plcol0 4;
  plline x middle_band;
  plcol0 1;
  plline x bottom_band;
  plend ()

let stochastic_plot k_line d_line =
  let temp_min =
    Array.fold_left (fun curr_min el -> min curr_min el) k_line.(0) k_line
  in
  let y_min =
    Array.fold_left (fun curr_min el -> min curr_min el) temp_min d_line
  in
  let temp_max =
    Array.fold_left (fun curr_min el -> max curr_min el) k_line.(0) k_line
  in
  let y_max =
    Array.fold_left (fun curr_min el -> max curr_min el) temp_max d_line
  in
  let x = Api.range_x (Array.length k_line) in
  plsfnam "stochastic.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2;
  plwidth 2.0;
  plenv 0.0 (float_of_int (Array.length x + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 3;
  plline x k_line;
  plcol0 5;
  plline x d_line;
  plend ()
