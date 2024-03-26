type t = 
  | String of string 
  | Int of int
  | Float of float
  | List of t list
  | Bool of bool
  | Association of (t * t) list

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