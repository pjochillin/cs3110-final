(* [t] represents the type that the graphing data is stored in *)
type t

(* [time_series ticker] returns the JSON object from the Alpha Vantage API about
   ticker [ticker] *)
val time_series : string -> Yojson.Safe.t

(* [polygon_series ticker] returns the JSON object from the Polygon.io API about
   ticker [ticker] *)
val polygon_series : string -> Yojson.Safe.t

(* [twelvedata_series ticker] returns the JSON object from the Twelve Data API
   about ticker [ticker] *)
val twelvedata_series : string -> Yojson.Safe.t

(* [apistocks_series ticker] returns the JSON object from the APIStocks API
   about ticker [ticker] *)
val apistocks_series : string -> Yojson.Safe.t

(* [assoc_of_json json] returns the graphing data from an Alpha Vantage
   [json] *)
val assoc_of_json : Yojson.Safe.t -> t

(* [assoc_of_polygon_json json] returns the graphing data from an Polygon.io
   [json] *)
val assoc_of_polygon_json : Yojson.Safe.t -> t

(* [assoc_of_twelvedata_json json] returns the graphing data from an Twelve Data
   [json] *)
val assoc_of_twelvedata_json : Yojson.Safe.t -> t

(* [assoc_of_apistocks_json json] returns the graphing data from an APIStocks
   [json] *)
val assoc_of_apistocks_json : Yojson.Safe.t -> t

(* [opens assoc] returns the open data from [assoc] *)
val opens : t -> float array

(* [highs assoc] returns the high data from [assoc] *)
val highs : t -> float array

(* [lows assoc] returns the low data from [assoc] *)
val lows : t -> float array

(* [closes assoc] returns the close data from [assoc] *)
val closes : t -> float array

(* [volumes assoc] returns the volume data from [assoc] *)
val volumes : t -> float array

(* [range_x i] returns an array with the numbers (in float form) from 0 to [i],
   exclusive *)
val range_x : int -> float array

(* [days_before date n] returns the date (in the form (year, month, day)) [n]
   days before [date]. Note: only exposed for use in testing *)
val days_before : int * int * int -> int -> int * int * int

(* [leap_year year] returns whether [year] is a leap year. Note: only exposed
   for use in testing *)
val leap_year : int -> bool
