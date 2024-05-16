(* [rsi lst period] calculates the RSI based on a stock close data in [lst] with
   period [period] *)
val rsi : float list -> int -> float list

(* [macd lst] calculates the MACD line and the corresponding signal line based
   on close data in [lst] *)
val macd : float list -> float list * float list

(* [obv vols close] calculates the OBV line based on a stock volume data in
   [vols] and close data in [close] *)
val obv : float list -> float list -> float list

(* [atr highs lows closes] calculates the ATR line based on a stock high data in
   [highs], low data in [lows], and close data in [closes] *)
val atr : float list -> float list -> float list -> float list

(* [cci highs lows closes period] calculates the CCI line based on a stock high
   data [highs], low data in [lows], close data in [closes], and period
   [period] *)
val cci : float list -> float list -> float list -> int -> float list

(* [bollinger_bands data period] calculates the lower, middle, and upper band
   for stock data in [data] and a period [period] *)
val bollinger_bands : float list -> int -> float list * float list * float list

(* [stochastic_oscillator highs lows closes period_k period_d] calculates the
   stochastic oscillator based on a stock high data [highs], low data in [lows],
   close data in [closes], and periods of k [period_k] (current price in
   percent) and periods of d [period_d] (3-day average of k) *)
val stochastic_oscillator :
  float list ->
  float list ->
  float list ->
  int ->
  int ->
  float list * float list

(* [take i lst] returns a list with the first [i] elements from [lst]. Note:
   only exposed for use in testing *)
val take : int -> 'a list -> 'a list

(* [sub lst i j] splices the list and grabs [j] elements starting at index [i]
   of [lst]]. Note: only exposed for use in testing *)
val sub : 'a list -> int -> int -> 'a list

(* [calculate_average lst] returns the average of the floats in [lst]. Note:
   only exposed for use in testing *)
val calculate_average : float list -> float

(* [calculate_mean_deviation lst sma _] returns the deviation of the mean of
   [lst] with SMA [sma] *)
val calculate_mean_deviation : float list -> float -> 'a -> float
