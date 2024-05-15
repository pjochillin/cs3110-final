val ticker_plot :
  float array ->
  float array ->
  float array ->
  float array ->
  float array ->
  unit
(** [make_plot x open high low close] creates a candlestick chart using Plplot
    based on the arguments provided *)

val macd_plot : float array -> float array -> unit
(** [macd_plot macd signal] creates a MACD graph that plots both the MACD line
    and the signal line *)

val rsi_plot : float array -> unit
(** [rsi_plot rsi] creates an RSI graph that plots the RSI data provided*)

val obv_plot : float array -> unit
(** [obv_plot obv] creates an OBV graph that plots the OBV data provided*)

val atr_plot : float array -> unit
(** [atr_plot obv] creates an OBV graph that plots the ATR data provided*)

val cci_plot : float array -> unit
(** [cci_plot cci_line] creates a CCI graph that plots the CCI data provided*)

val bollinger_bands_plot : float array -> float array -> float array -> unit
(** [bollinger_plot top_band middle_band bottom_band] creates a Bollinger Bands
    graph that plots the top, middle, and bottom lines data provided *)

val stochastic_plot : float array -> float array -> unit
(** [stochastic_plot k_line d_line] creates a Stochastic Oscillator graph that
    plots the K line and the D line *)
