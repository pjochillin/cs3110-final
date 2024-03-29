open Plplot

let make_plot x y =
  let y_min = Array.fold_left (fun curr_min el -> min curr_min el) y.(0) y in
  let y_max = Array.fold_left (fun curr_min el -> max curr_min el) y.(0) y in
  plinit ();
  plcol0 2; 
  plenv 0.0 (float_of_int ((Array.length x) + 2)) (y_min -. 10.) (y_max +. 10.) 0 0;
  plcol0 3;
  plline x y;
  plend ();

