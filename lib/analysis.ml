let take n lst =
  let rec aux i acc = function
    | [] -> List.rev acc
    | x :: xs -> if i = 0 then List.rev acc else aux (i - 1) (x :: acc) xs
  in
  aux n [] lst

let sub lst start len =
  let rec drop n l =
    match (n, l) with 0, _ -> l | _, [] -> [] | n, _ :: xs -> drop (n - 1) xs
  in
  take len (drop start lst)

let calculate_average lst =
  let sum = List.fold_left ( +. ) 0. lst in
  sum /. float_of_int (List.length lst)

let calculate_rsi close_prices period =
  let rec calculate_gains_losses prev_close gains losses = function
    | [] -> (gains, losses)
    | close :: tail ->
        let change = close -. prev_close in
        if change > 0. then
          calculate_gains_losses close (change :: gains) losses tail
        else calculate_gains_losses close gains (-.change :: losses) tail
  in
  let gains, losses =
    match close_prices with
    | [] -> ([], [])
    | first :: rest -> calculate_gains_losses first [] [] rest
  in
  let avg_gain = calculate_average (take period gains) in
  let avg_loss = calculate_average (take period losses) in
  let rs = if avg_loss = 0. then 100. else avg_gain /. avg_loss in
  let rsi = 100. -. (100. /. (1. +. rs)) in
  rsi

let rsi close_data period =
  let rec aux acc i =
    if i < period then acc
    else
      let subset = sub close_data (i - period) period in
      let rsi = calculate_rsi subset period in
      aux (rsi :: acc) (i - 1)
  in
  List.rev (aux [] (List.length close_data))

let ema period prices =
  let alpha = 2. /. (Float.of_int period +. 1.) in
  let rec aux ema_values prev_ema = function
    | [] -> List.rev ema_values
    | p :: ps ->
        let ema_today = (alpha *. p) +. ((1. -. alpha) *. prev_ema) in
        aux (ema_today :: ema_values) ema_today ps
  in
  match prices with hd :: tl -> aux [ hd ] hd tl | [] -> []

(* Calculate MACD line as the difference between 12-day EMA and 26-day EMA *)
let calculate_macd prices =
  let ema12 = ema 12 prices in
  let ema26 = ema 26 prices in
  List.map2 ( -. ) ema12 ema26

(* Calculate the signal line as the 9-day EMA of the MACD line *)
let signal_line macd_line = ema 9 macd_line

(* Function to process all MACD calculations and prepare for plotting or analysis *)
let macd close_data =
  let macd_line = calculate_macd close_data in
  let signal = signal_line macd_line in
  (macd_line, signal)
