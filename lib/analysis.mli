type t

val assoc_of_json : Yojson.Safe.t -> t

val opens : t -> float list

val highs : t -> float list

val lows : t -> float list

val closes : t -> float list

val volumes : t -> float list

val range_x : int -> int list