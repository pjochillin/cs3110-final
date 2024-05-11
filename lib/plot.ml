open Plplot

let ticker_plot x open_price high_price low_price close_price =
  let y_min = Array.fold_left (fun curr_min el -> min curr_min el) high_price.(0) high_price in
  let y_max = Array.fold_left (fun curr_min el -> max curr_min el) high_price.(0) high_price in
  plsfnam "plot.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2; 
  plenv 0.0 (float_of_int ((Array.length x) + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 3;
  Array.iteri (fun i _ ->
    let i_f = float_of_int i in
    plline [|i_f; i_f|] [|low_price.(i); high_price.(i)|];
    
    plline [|i_f -. 0.2; i_f +. 0.2|] [|open_price.(i); open_price.(i)|];
    plline [|i_f -. 0.2; i_f +. 0.2|] [|close_price.(i); close_price.(i)|];
    
    if close_price.(i) >= open_price.(i) then
      plfill [|i_f -. 0.2; i_f +. 0.2; i_f +. 0.2; i_f -. 0.2|] [|open_price.(i); open_price.(i); close_price.(i); close_price.(i)|]
    else
      plfill [|i_f -. 0.2; i_f +. 0.2; i_f +. 0.2; i_f -. 0.2|] [|close_price.(i); close_price.(i); open_price.(i); open_price.(i)|];
  ) x;
  plend ()

let macd_plot macd_line signal_line = 
  let temp_min = Array.fold_left (fun curr_min el -> min curr_min el) macd_line.(0) macd_line in
  let y_min = Array.fold_left (fun curr_min el -> min curr_min el) temp_min signal_line in
  let temp_max = Array.fold_left (fun curr_min el -> max curr_min el) macd_line.(0) macd_line in
  let y_max = Array.fold_left (fun curr_min el -> max curr_min el) temp_max signal_line in
  let x = Api.range_x (Array.length macd_line) in
  plsfnam "macd.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2; 
  plwidth 2.0;
  plenv 0.0 (float_of_int ((Array.length x) + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 3;
  plline x macd_line;
  plcol0 5;
  plline x signal_line;
  plend ()


let rsi_plot rsi_line = 
  let y_min = Array.fold_left (fun curr_min el -> min curr_min el) rsi_line.(0) rsi_line in
  let y_max = Array.fold_left (fun curr_min el -> max curr_min el) rsi_line.(0) rsi_line in
  let x = Api.range_x (Array.length rsi_line) in
  plsfnam "rsi.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2; 
  plwidth 2.0;
  plenv 0.0 (float_of_int ((Array.length x) + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 3;
  plline x rsi_line;
  plend ()

  
let obv_plot obv_line = 
  let y_min = Array.fold_left (fun curr_min el -> min curr_min el) obv_line.(0) obv_line in
  let y_max = Array.fold_left (fun curr_min el -> max curr_min el) obv_line.(0) obv_line in
  let x = Api.range_x (Array.length obv_line) in
  plsfnam "obv.jpeg";
  plsdev "jpgqt";
  plspage 0.0 0.0 2400 1800 1000 1000;
  plinit ();
  plcol0 2; 
  plwidth 2.0;
  plenv 0.0 (float_of_int ((Array.length x) + 2)) (y_min -. 1.) (y_max +. 1.) 0 0;
  plcol0 9;
  plline x obv_line;
  plend ()