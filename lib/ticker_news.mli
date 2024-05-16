(* [fetch_ticker_json ticker] returns the JSON from TickerTick based on ticker
   [ticker] Note: only exposed for use in testing *)
val fetch_ticker_json : string -> Yojson.Safe.t

(* [assoc_of_ticker_json json] returns the structured data from [json] Note:
   only exposed for use in testing *)
val assoc_of_ticker_json : Yojson.Safe.t -> (string * string) list list

(* [fetch_news ticker] returns the structured data from TickerTick based on
   ticker [ticker] *)
val fetch_news : string -> (string * string) list list Lwt.t
