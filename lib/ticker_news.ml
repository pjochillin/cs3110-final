open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

(* Define a type for news items *)
type news_item = {
  title : string;
  url : string;
}

(* Function to fetch news for a given ticker and return a list of news items *)
let fetch_news ticker =
  let base_url = "https://api.tickertick.com/api/news/all" in
  let url = Uri.of_string (base_url ^ "?symbols=" ^ ticker) in

  Client.get url >>= fun (resp, body) ->
  match resp.status with
  | `OK ->
      Cohttp_lwt.Body.to_string body >>= fun body_string ->
      let json = Yojson.Basic.from_string body_string in
      let news_items =
        json |> member "news" |> to_list
        |> List.map (fun news_item ->
               {
                 title = news_item |> member "title" |> to_string;
                 url = news_item |> member "url" |> to_string;
               })
      in
      (* Print each news item *)
      List.iter
        (fun item -> Printf.printf "Title: %s\nURL: %s\n\n" item.title item.url)
        news_items;
      Lwt.return news_items
  | _ ->
      Printf.printf "Failed to fetch news for ticker: %s\n" ticker;
      Lwt.return [] (* Return an empty list in case of HTTP error *)

(* Main function to drive the program *)
let () =
  let ticker = "AAPL" in
  Lwt_main.run
    ( fetch_news ticker >>= fun news_items ->
      (* Process the list of news items here if needed *)
      List.iter
        (fun item -> Printf.printf "Title: %s\nURL: %s\n\n" item.title item.url)
        news_items;
      Lwt.return_unit )
