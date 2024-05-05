type t

val key : string

val time_series : string -> Yojson.Safe.t

val assoc_of_json : Yojson.Safe.t -> t

val opens : t -> float array

val highs : t -> float array

val lows : t -> float array

val closes : t -> float array

val volumes : t -> float array

val range_x : int -> float array