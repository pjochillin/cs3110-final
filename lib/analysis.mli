val rsi : float list -> int -> float list
val macd : float list -> float list * float list
val obv : float list -> float list -> float list
val atr : float list -> float list -> float list -> float list
val cci : float list -> float list -> float list -> int -> float list
val bollinger_bands : float list -> int -> float list * float list * float list
val stochastic_oscillator : float list -> int -> int -> float list * float list
val take : int -> 'a list -> 'a list
val sub : 'a list -> int -> int -> 'a list
val calculate_average : float list -> float
val calculate_mean_deviation : float list -> float -> 'a -> float
