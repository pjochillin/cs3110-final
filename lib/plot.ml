open Plplot

let make_plot x open_price high_price low_price close_price =
  let y_min = Array.fold_left (fun curr_min el -> min curr_min el) high_price.(0) high_price in
  let y_max = Array.fold_left (fun curr_min el -> max curr_min el) high_price.(0) high_price in
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
  plend ();

