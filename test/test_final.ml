open Final
open OUnit2
(* open QCheck *)

(* Specific examples from trusted sources, separate data in separate files*)
let closes =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv_closes.txt"))

let closes2 =
  List.map float_of_string
    (BatList.of_enum (BatFile.lines_of "obv2_closes.txt"))

let vols =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv_vols.txt"))

let vols2 =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "obv2_vols.txt"))

let atr_highs =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "atr_highs.txt"))

let atr_lows =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "atr_lows.txt"))

let atr_closes =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "atr_closes.txt"))

let cci_highs =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "cci_highs.txt"))

let cci_lows =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "cci_lows.txt"))

let cci_closes =
  List.map float_of_string (BatList.of_enum (BatFile.lines_of "cci_closes.txt"))

let bollinger_closes =
  List.map float_of_string
    (BatList.of_enum (BatFile.lines_of "bollinger_data.txt"))

let alphavantage_data = Yojson.Safe.from_file "alphavantage_data.txt"
let polygon_data = Yojson.Safe.from_file "polygon_data.txt"
let twelvedata_data = Yojson.Safe.from_file "twelvedata_data.txt"
let apistocks_data = Yojson.Safe.from_file "apistocks_data.txt"

(* RSI data parser *)
let rsi_fun line =
  let split = String.split_on_char ' ' line in
  match split with
  | [] -> failwith "Empty line found in RSI file read!"
  | h :: t ->
      let period = int_of_string h in
      let closes = List.map float_of_string t in
      (period, closes)

let rsi_data =
  BatList.of_enum (BatFile.lines_of "rsi_data.txt") |> List.map rsi_fun

let tests =
  [
    (* This test originates from
       https://www.investopedia.com/terms/o/onbalancevolume.asp *)
    ( "obv_test" >:: fun _ ->
      assert_equal 72100. (Analysis.obv vols closes |> List.rev |> List.hd) );
    (* This test originates from
       https://school.stockcharts.com/doku.php?id=technical_indicators:on_balance_volume_obv *)
    ( "obv_test 2" >:: fun _ ->
      assert_equal 54300. (Analysis.obv vols2 closes2 |> List.rev |> List.hd) );
    (* This test originates from
       https://www.investopedia.com/terms/a/atr.asp#toc-how-to-calculate-the-atr *)
    ( "atr_test" >:: fun _ ->
      assert_equal 1.19
        (List.hd (List.rev (Analysis.atr atr_highs atr_lows atr_closes))) );
    (* This test and data originates from
       https://school.stockcharts.com/doku.php?id=technical_indicators:commodity_channel_index_cci*)
    ( "cci_test 1" >:: fun _ ->
      assert_equal 102
        (Analysis.cci cci_highs cci_lows cci_closes 20
        |> List.rev |> List.hd |> int_of_float);
      assert_equal (-73)
        (Analysis.cci cci_highs cci_lows cci_closes 20
        |> List.hd |> int_of_float) );
    ( "bollinger_test" >:: fun _ ->
      let middle_band, upper_band, lower_band =
        Analysis.bollinger_bands bollinger_closes 20
      in
      assert_equal 94 (upper_band |> List.rev |> List.hd |> int_of_float);
      assert_equal 91 (middle_band |> List.rev |> List.hd |> int_of_float);
      assert_equal 87 (lower_band |> List.rev |> List.hd |> int_of_float) );
    ( "days_before_test same" >:: fun _ ->
      assert_equal (2024, 5, 12) (Api.days_before (2024, 5, 12) 0) );
    ( "days_before_test same month" >:: fun _ ->
      assert_equal (2024, 5, 9) (Api.days_before (2024, 5, 12) 3) );
    ( "days_before_test back month" >:: fun _ ->
      assert_equal (2024, 4, 27) (Api.days_before (2024, 5, 12) 15) );
    ( "days_before_test back multiple month" >:: fun _ ->
      assert_equal (2024, 3, 31) (Api.days_before (2024, 5, 12) 42) );
    ( "days_before_test back year" >:: fun _ ->
      assert_equal (2023, 12, 22) (Api.days_before (2024, 1, 1) 10) );
    ( "days_before_test leap year" >:: fun _ ->
      assert_equal (2024, 2, 28) (Api.days_before (2024, 3, 1) 2) );
    ( "days_before_test not leap year" >:: fun _ ->
      assert_equal (2023, 2, 27) (Api.days_before (2023, 3, 1) 2) );
    ("leap_year_test normal" >:: fun _ -> assert_equal true (Api.leap_year 2020));
    ( "leap_year_test normal not" >:: fun _ ->
      assert_equal false (Api.leap_year 2021) );
    ( "leap_year_test century not" >:: fun _ ->
      assert_equal false (Api.leap_year 1900) );
    ( "leap_year_test century" >:: fun _ ->
      assert_equal true (Api.leap_year 2000) );
    ( "leap_year_test normal 2" >:: fun _ ->
      assert_equal true (Api.leap_year 1996) );
    ( "leap_year_test normal not 2" >:: fun _ ->
      assert_equal false (Api.leap_year 1999) );
    ("take_test zero" >:: fun _ -> assert_equal [] (Analysis.take 0 []));
    ("take_test empty" >:: fun _ -> assert_equal [] (Analysis.take 3 []));
    ( "take_test normal" >:: fun _ ->
      assert_equal [ 1; 2; 3 ] (Analysis.take 3 [ 1; 2; 3; 4 ]) );
    ( "take_test normal 2" >:: fun _ ->
      assert_equal [ 1 ] (Analysis.take 1 [ 1; 2; 3; 4 ]) );
    ( "take_test entire" >:: fun _ ->
      assert_equal [ 1; 2; 3; 4 ] (Analysis.take 4 [ 1; 2; 3; 4 ]) );
    ( "take_test entire 2" >:: fun _ ->
      assert_equal [ 1; 2; 3 ] (Analysis.take 3 [ 1; 2; 3 ]) );
    ( "take_test more" >:: fun _ ->
      assert_equal [ 1; 4; 9 ] (Analysis.take 10 [ 1; 4; 9 ]) );
    ("sub_test zero" >:: fun _ -> assert_equal [] (Analysis.sub [] 0 0));
    ("sub_test zero 2" >:: fun _ -> assert_equal [] (Analysis.sub [ 1; 3 ] 0 0));
    ("sub_test empty" >:: fun _ -> assert_equal [] (Analysis.sub [ 1; 4 ] 1 0));
    ( "sub_test normal" >:: fun _ ->
      assert_equal [ 4 ] (Analysis.sub [ 1; 4 ] 1 1) );
    ( "sub_test normal 2" >:: fun _ ->
      assert_equal [ 4; 5 ] (Analysis.sub [ 1; 2; 3; 4; 5 ] 3 2) );
    ( "sub_test full" >:: fun _ ->
      assert_equal [ 1; 2; 3; 4; 5 ] (Analysis.sub [ 1; 2; 3; 4; 5 ] 0 5) );
    ( "sub_test full 2" >:: fun _ ->
      assert_equal [ 2; 3; 4; 5 ] (Analysis.sub [ 1; 2; 3; 4; 5 ] 1 4) );
    ( "sub_test more" >:: fun _ ->
      assert_equal [ 2; 3; 4; 5 ] (Analysis.sub [ 1; 2; 3; 4; 5 ] 1 5) );
    ( "calculate_average_test" >:: fun _ ->
      assert_equal 3. (Analysis.calculate_average [ 1.; 2.; 3.; 4.; 5. ]) );
    ( "calculate_average_test 2" >:: fun _ ->
      assert_equal 43.85
        (Analysis.calculate_average [ 50.; 49.; 70.5; 2.25; 47.5 ]) );
    ( "calculate_average_test singleton" >:: fun _ ->
      assert_equal 1.5 (Analysis.calculate_average [ 1.5 ]) );
    ("range_x_test 0" >:: fun _ -> assert_equal [||] (Api.range_x 0));
    ("range_x_test 1" >:: fun _ -> assert_equal [| 0. |] (Api.range_x 1));
    ( "range_x_test many" >:: fun _ ->
      assert_equal [| 0.; 1.; 2.; 3.; 4.; 5.; 6. |] (Api.range_x 7) );
    (* Note: for the tests below, they can potentially fail if the API service
       has hit its maximum number of requests in a certain period (eg. Alpha
       Vantage -> 25/day) *)
    (* ("alphavantage_series_test" >:: fun _ -> assert_equal true
       (Api.time_series "AAPL" |> Yojson.Safe.Util.keys |> List.mem "Time Series
       (60min)")); ("polygon_series_test" >:: fun _ -> assert_equal true
       (Api.polygon_series "AAPL" |> Yojson.Safe.Util.keys |> List.mem
       "results")); ("twelvedata_series_test" >:: fun _ -> assert_equal true
       (Api.twelvedata_series "AAPL" |> Yojson.Safe.Util.keys |> List.mem
       "values")); ("apistocks_series_test" >:: fun _ -> assert_equal true
       (Api.apistocks_series "AAPL" |> Yojson.Safe.Util.keys |> List.mem
       "Results")); *)
    ( "alphavantage_opens_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_json alphavantage_data |> Api.opens).(0) = 182.6500) );
    ( "alphavantage_closes_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_json alphavantage_data |> Api.closes).(0) = 182.5100) );
    ( "alphavantage_highs_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_json alphavantage_data |> Api.highs).(0) = 182.6500) );
    ( "alphavantage_lows_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_json alphavantage_data |> Api.lows).(0) = 182.5000) );
    ( "alphavantage_volume_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_json alphavantage_data |> Api.volumes).(0) = 4936.) );
    ( "polygon_opens_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_polygon_json polygon_data |> Api.opens).(0) = 170.5) );
    ( "polygon_closes_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_polygon_json polygon_data |> Api.closes).(0) = 171.12) );
    ( "polygon_highs_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_polygon_json polygon_data |> Api.highs).(0) = 171.5) );
    ( "polygon_lows_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_polygon_json polygon_data |> Api.lows).(0) = 170.5) );
    ( "polygon_volume_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_polygon_json polygon_data |> Api.volumes).(0) = 54112.)
    );
    ( "twelvedata_opens_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_twelvedata_json twelvedata_data |> Api.opens).(0)
        = 182.94000) );
    ( "twelvedata_closes_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_twelvedata_json twelvedata_data |> Api.closes).(0)
        = 183.00000) );
    ( "twelvedata_highs_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_twelvedata_json twelvedata_data |> Api.highs).(0)
        = 183.20000) );
    ( "twelvedata_lows_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_twelvedata_json twelvedata_data |> Api.lows).(0)
        = 182.78999) );
    ( "twelvedata_volume_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_twelvedata_json twelvedata_data |> Api.volumes).(0)
        = 5183512.) );
    ( "apistocks_opens_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_apistocks_json apistocks_data |> Api.opens).(0)
        = 183.990005) );
    ( "apistocks_closes_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_apistocks_json apistocks_data |> Api.closes).(0)
        = 186.860001) );
    ( "apistocks_highs_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_apistocks_json apistocks_data |> Api.highs).(0)
        = 186.949997) );
    ( "apistocks_lows_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_apistocks_json apistocks_data |> Api.lows).(0)
        = 183.820007) );
    ( "apistocks_volume_test" >:: fun _ ->
      assert_equal true
        ((Api.assoc_of_apistocks_json apistocks_data |> Api.volumes).(0)
        = 64885400.) );
  ]

let test_suite = "CS3110 Final Test Suite" >::: tests
let _ = run_test_tt_main test_suite
