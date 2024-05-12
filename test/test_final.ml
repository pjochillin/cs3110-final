open Final
open OUnit2
(* open QCheck *)

(* Specific examples from trusted sources, separate data in separate files*)
let closes = List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv_closes.txt"))
let closes2 = List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv2_closes.txt"))
let vols = List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv_vols.txt"))
let vols2 = List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv2_vols.txt"))
let atr_highs = List.map float_of_string (BatList.of_enum (BatFile.lines_of "atr_highs.txt"))
let atr_lows = List.map float_of_string (BatList.of_enum (BatFile.lines_of "atr_lows.txt"))
let atr_closes = List.map float_of_string (BatList.of_enum (BatFile.lines_of "atr_closes.txt"))

(* RSI data parser *)
let rsi_fun line =
  let split = String.split_on_char ' ' line in
  match split with
  | [] -> failwith "Empty line found in RSI file read!"
  | h :: t ->
    let period = int_of_string h in
    let closes = List.map float_of_string t in
    period, closes
let rsi_data = (BatList.of_enum (BatFile.lines_of "rsi_data.txt"))
  |> List.map rsi_fun

let tests =
  [
    (* This test originates from https://www.investopedia.com/terms/o/onbalancevolume.asp *)
    ("obv_test" >:: fun _ -> assert_equal 72100. (List.hd (List.rev (Analysis.obv vols closes))));
    (* This test originates from https://school.stockcharts.com/doku.php?id=technical_indicators:on_balance_volume_obv *)
    ("obv_test 2" >:: fun _ -> assert_equal 54300. (List.hd (List.rev (Analysis.obv vols2 closes2))));
    (* This test originates from https://www.investopedia.com/terms/a/atr.asp#toc-how-to-calculate-the-atr *)
    ("atr_test") >:: fun _ -> assert_equal 1.19 (List.hd (List.rev (Analysis.atr atr_highs atr_lows atr_closes)));

  ]

let test_suite = "CS3110 Final Test Suite" >::: tests
let _ = run_test_tt_main test_suite
