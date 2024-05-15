open Lwt
open Cohttp_lwt_unix
open Yojson.Safe.Util

let fetch_ticker_json ticker =
  let base_url = "https://api.tickertick.com/feed?q=(and T:curated tt:" in
  let body =
    Client.get (Uri.of_string (base_url ^ ticker ^ ")&n=5"))
    >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
  in
  Yojson.Safe.from_string (Lwt_main.run body)

let assoc_of_ticker_json json =
  let data = member "stories" json |> to_list in
  let extract_data el =
    let title = member "title" el |> to_string in
    let url = member "url" el |> to_string in
    [ ("title", title); ("url", url) ]
  in
  List.map extract_data data

let fetch_news ticker =
  let data = fetch_ticker_json ticker in
  let results = assoc_of_ticker_json data in
  Lwt.return results
