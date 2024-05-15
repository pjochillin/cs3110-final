type t

val time_series : string -> Yojson.Safe.t
val polygon_series : string -> Yojson.Safe.t
val twelvedata_series : string -> Yojson.Safe.t
val apistocks_series : string -> Yojson.Safe.t
val assoc_of_json : Yojson.Safe.t -> t
val assoc_of_polygon_json : Yojson.Safe.t -> t
val assoc_of_twelvedata_json : Yojson.Safe.t -> t
val assoc_of_apistocks_json : Yojson.Safe.t -> t
val opens : t -> float array
val highs : t -> float array
val lows : t -> float array
val closes : t -> float array
val volumes : t -> float array
val range_x : int -> float array
val days_before : int * int * int -> int -> int * int * int
val leap_year : int -> bool
