open Plplot

let make_plot x y =
  plinit ();
  plcol0 2; 
  plenv 0.0 6.0 0.0 30.0 0 0; (* TODO: update boundaries based on arrays *)
  plcol0 3;
  plline x y;
  plend ();

