(* [make_plot x open high low close] creates a candlestick chart using Plplot based on the arguments provided *)
val ticker_plot : float array -> float array -> float array -> float array -> float array -> unit

(* [macd_plot macd signal] creates a MACD graph that plots both the MACD line and the signal line *)
val macd_plot : float array -> float array -> unit

(* [rsi_plot rsi] creates an RSI graph that plots the RSI data provided*)
val rsi_plot : float array -> unit

(* [obv_plot obv] creates an OBV graph that plots the OBV data provided*)
val obv_plot : float array -> unit

(* [atr_plot obv] creates an OBV graph that plots the ATR data provided*)
val atr_plot : float array -> unit