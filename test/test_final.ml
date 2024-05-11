open Final
open OUnit2
(* open QCheck *)

let closes =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv_closes.txt"))

let closes2 =
  List.map float_of_string
    (BatList.of_enum (BatFile.lines_of "obv2_closes.txt"))

let vols =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv_vols.txt"))

let vols2 =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv2_vols.txt"))

let tests =
  [
    (* This test originates from https://www.investopedia.com/terms/o/onbalancevolume.asp *)
    ("obv_test" >:: fun _ -> assert_equal 72100. (List.hd (List.rev (Analysis.obv vols closes))));
    (* This test originates from https://school.stockcharts.com/doku.php?id=technical_indicators:on_balance_volume_obv *)
    ("obv_test 2" >:: fun _ -> assert_equal 54300. (List.hd (List.rev (Analysis.obv vols2 closes2))));
  ]

let test_suite = "CS3110 Final Test Suite" >::: tests
let _ = run_test_tt_main test_suite
