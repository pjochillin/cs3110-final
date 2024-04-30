type financial_status =
  | Good
  | Bad

type t = 
  | String of string 
  | Int of int
  | Float of float
  | List of t list
  | Bool of bool
  | Association of (t * t) list
  | FinancialStatus of financial_status

  let visualize_calendar (data : t) =
    match data with
    | List rows ->
      List.iter (fun row ->
        match row with
        | List cols ->
          List.iter (fun cell ->
            match cell with
            | Float value ->
              if value > 0.0 then print_string "\x1b[32m"  (* Green color for positive values *)
              else if value < 0.0 then print_string "\x1b[31m"  (* Red color for negative values *)
              else print_string "\x1b[0m";  (* Reset color *)
              Printf.printf "%.2f " value;
              print_string "\x1b[0m"  (* Reset color *)
            | _ -> ()
          ) cols;
          print_endline ""
        | _ -> ()
      ) rows
    | _ -> ()  (* Handle cases where the data is not a list *)
  
let rec parse_json str = 
  let str = String.trim str in
  if String.starts_with ~prefix:"{" str && String.ends_with ~suffix:"}" str then
    if String.length str = 2 then 
      Association [] 
    else
      let insides = String.sub str 1 (String.length str - 2) in
      let pairs = String.split_on_char ',' (String.trim insides) in 
      Association (List.fold_left (fun acc x -> 
        let split_kv = String.split_on_char ':' (String.trim x) in
          (parse_json (String.trim (List.nth split_kv 0)), parse_json (String.trim (List.nth split_kv 1))) :: acc
        ) [] pairs)
  else if String.starts_with ~prefix:"[" str && String.ends_with ~suffix:"]" str then
    if String.length str = 2 then 
      List [] 
    else
      let insides = String.sub str 1 (String.length str - 2) in
      let items = String.split_on_char ',' insides in
      List (List.rev (List.fold_left (fun acc x -> parse_json (String.trim x) :: acc) [] items))
  else if str = "\"true\"" then Bool true
  else if str = "\"false\"" then Bool false
  else if int_of_string_opt str <> None then Int (int_of_string str)
  else if float_of_string_opt (String.trim str) <> None then Float (float_of_string str)
  else if String.starts_with ~prefix:"\"" str && String.ends_with ~suffix:"\"" str then
    String (String.sub str 1 (String.length str - 2))
  else failwith "Invalid JSON String"

let color_code_data (data : t) =
  (* Color coding logic here *)
  match data with
  | Float f ->
    if f > 0.0 then FinancialStatus Good
    else FinancialStatus Bad
  | _ -> failwith "Invalid data type for color coding"


let rec find_data_in_time_period (data : t) start_date end_date =
  match data with
  | Association assoc_list ->
    let filtered_assoc_list = List.map (fun (key, value) ->
      (key, find_data_in_time_period value start_date end_date)
    ) assoc_list in
    Association filtered_assoc_list

  | List lst ->
    let filtered_lst = List.map (fun item ->
      find_data_in_time_period item start_date end_date
    ) lst in
    List filtered_lst

  | _ -> data  (* For primitive data types, no need to filter based on time period *)

let user_interactive_view (data : t) time_period =
  (* User interaction logic *)
  let (start_date, end_date) = time_period in
  let data_in_period = find_data_in_time_period data start_date end_date in
  visualize_calendar data_in_period
