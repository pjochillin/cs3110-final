let take n lst =
  let rec aux i acc = function
    | [] -> List.rev acc
    | x :: xs -> if i = 0 then List.rev acc else aux (i - 1) (x :: acc) xs
  in
  aux n [] lst

let sub lst start len =
  let rec drop n l =
    match (n, l) with
    | _, [] -> []
    | 0, _ -> l
    | n, _ :: xs -> drop (n - 1) xs
  in
  take len (drop start lst)

let calculate_average lst =
  let sum = List.fold_left ( +. ) 0. lst in
  sum /. float_of_int (List.length lst)

let calculate_mean_deviation data sma period =
  let deviations = List.map (fun x -> abs_float (x -. sma)) data in
  calculate_average deviations

let calculate_standard_deviation data sma =
  let squared_diffs = List.map (fun x -> (x -. sma) ** 2.) data in
  let variance = calculate_average squared_diffs in
  sqrt variance

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
  match prices with
  | [] -> []
  | hd :: tl -> aux [ hd ] hd tl

(* Helper: calculate MACD line as the difference between 12-period EMA and
   26-period EMA *)
let calculate_macd prices =
  let ema12 = ema 12 prices in
  let ema26 = ema 26 prices in
  List.map2 ( -. ) ema12 ema26

(* Helper: calculate the signal line as the 9-period EMA of the MACD line *)
let signal_line macd_line = ema 9 macd_line

(* Function to process all MACD calculations and prepare for plotting or
   analysis *)
let macd close_data =
  let macd_line = calculate_macd close_data in
  let signal = signal_line macd_line in
  (macd_line, signal)

let obv vols closes =
  let rec obv_helper acc yest v c =
    match (v, c) with
    | [], [] -> List.rev acc
    | h1 :: t1, h2 :: t2 ->
        if h2 > yest then obv_helper ((List.hd acc +. h1) :: acc) h2 t1 t2
        else if h2 < yest then obv_helper ((List.hd acc -. h1) :: acc) h2 t1 t2
        else obv_helper (List.hd acc :: acc) h2 t1 t2
    | _ -> failwith "Arrays not equal size for calculating OBV!"
  in
  obv_helper [ 0. ] (List.hd closes) vols closes

let atr highs lows closes =
  let float_of_some = function
    | None -> failwith "Expected non-none value!"
    | Some x -> x
  in
  let rec atr_helper acc yest his los cls acc' =
    if yest = None then
      match (his, los, cls) with
      | _ :: hi_tl, _ :: lo_tl, cl :: cl_tl ->
          atr_helper acc (Some cl) hi_tl lo_tl cl_tl acc'
      | _ -> failwith "Arrays must not be initialized empty!"
    else
      match (his, los, cls) with
      | [], [], [] ->
          List.rev ((acc /. (float_of_int (List.length closes) -. 1.)) :: acc')
      | hi :: hi_tl, lo :: lo_tl, cl :: cl_tl ->
          let op1 = hi -. lo in
          let op2 = abs_float (hi -. float_of_some yest) in
          let op3 = abs_float (lo -. float_of_some yest) in
          let max_val = max op1 (max op2 op3) in
          atr_helper (acc +. max_val) (Some cl) hi_tl lo_tl cl_tl
            (((acc +. max_val) /. float_of_int (List.length acc' + 1)) :: acc')
      | _ -> failwith "Arrays not equal size for calculating ATR!"
  in
  atr_helper 0. None highs lows closes []

let cci high low close period =
  let high_low = List.map2 (fun h l -> h +. l) high low in
  let tp = List.map2 (fun hl c -> (hl +. c) /. 3.) high_low close in

  let rec aux cci_values i =
    if i < period then List.rev cci_values
    else
      let subset = sub tp (i - period) period in
      let sma = calculate_average subset in
      let mean_dev = calculate_mean_deviation subset sma period in
      let cci_value =
        if mean_dev = 0. then 0.
        else (List.nth tp (i - 1) -. sma) /. (0.015 *. mean_dev)
      in
      aux (cci_value :: cci_values) (i - 1)
  in
  aux [] (List.length tp)

let bollinger_bands data period =
  let rec aux middle_band upper_band lower_band i =
    if i < period then
      (List.rev middle_band, List.rev upper_band, List.rev lower_band)
    else
      let subset = sub data (i - period) period in
      let sma = calculate_average subset in
      let std_dev = calculate_standard_deviation subset sma in
      let upper_band_value = sma +. (2. *. std_dev) in
      let lower_band_value = sma -. (2. *. std_dev) in
      aux (sma :: middle_band)
        (upper_band_value :: upper_band)
        (lower_band_value :: lower_band)
        (i - 1)
  in
  aux [] [] [] (List.length data)

(* Stochastic %K calculation based on highs, lows, and closes *)
let stochastic_k highs lows closes period =
  let rec aux k_values i =
    if i < period then List.rev k_values
    else
      let high_subset = sub highs (i - period) period in
      let low_subset = sub lows (i - period) period in
      let close = List.nth closes (i - 1) in
      let highest_high = List.fold_left max (List.hd high_subset) high_subset in
      let lowest_low = List.fold_left min (List.hd low_subset) low_subset in
      let k_value =
        (close -. lowest_low) /. (highest_high -. lowest_low) *. 100.
      in
      aux (k_value :: k_values) (i - 1)
  in
  aux [] (List.length closes)

let simple_moving_average period prices =
  let rec aux sma_values i =
    if i < period then List.rev sma_values
    else
      let subset = sub prices (i - period) period in
      let sma = calculate_average subset in
      aux (sma :: sma_values) (i - 1)
  in
  aux [] (List.length prices)

let stochastic_d k_values period =
  let k_sma = simple_moving_average period k_values in
  k_sma

let stochastic_oscillator highs lows closes period_k period_d =
  let k_values = stochastic_k highs lows closes period_k in
  let d_values = stochastic_d k_values period_d in
  (k_values, d_values)
