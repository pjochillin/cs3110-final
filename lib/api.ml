open Cohttp_lwt_unix
open Lwt

let key = "RGWPOZSX9QV9O03C"

let time_series ticker =
  let body =
    Client.get (Uri.of_string ("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=" ^ String.uppercase_ascii ticker ^ "&interval=5min&apikey=" ^ key)) >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body in
  Yojson.Safe.from_string (Lwt_main.run body)