(* CSE 341, HW2 Provided Code *)

(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
include Json

(* These come from the parsed_*_bus.ml.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (~1000 reports),
   respectively with the data that you will need to implement
   your homework.
*)
open Json_structures.Parsed_complete_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_small_bus

(* provided helper function that deduplicates a list *)
let dedup xs = List.sort_uniq compare xs

(* provided helper function that sorts a given list *)
let sort xs = List.sort compare xs

(* provided helper function to convert a float to a string *)
(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job how we want. *)
  
let json_string_of_float f =
  Printf.sprintf "%g" f

let some_to_item (item) =
    match item with
    | Some (x) -> x
    | None -> raise (Invalid_argument "Option.get")
;;

let rec recurse_for_silly_json(i : int) = 
  let ifloat = float_of_int(i) in
  if(i = 1) then [Object [("n", Num 1.0); ("b", True)]]
  else [Object [("n", Num ifloat); ("b", True)]]@recurse_for_silly_json(i-1)

(* 1 *)
let make_silly_json i =
  Array (recurse_for_silly_json(i))

(* 2 *)
let rec concat_with (sep, ss) =
  match ss with
    [] -> ""
    | hd::tl -> if(tl = []) then hd else hd ^ sep ^ concat_with (sep, tl)

(* 3 *)
let quote_string s =
  "\"" ^ s ^ "\""

let rec mult_to_single_json (lambda : ('a -> 'b)) (items : 'a list) : ('b list) =
    match items with
    | [] -> []
    | (hd :: tl) -> (lambda hd) :: (mult_to_single_json lambda tl)

(* 4 *)
let rec string_of_json j =
  let rec string_of_jsonobj (key_val) =
      match key_val with
      | (key, value) -> (quote_string(key))^" : "^(string_of_json(value))
  in
  match j with
    Num num -> json_string_of_float(num)
    | String str -> quote_string(str)
    | False -> "false"
    | True -> "true"
    | Null -> "null"
    | Array (json) -> "["^concat_with(", ", mult_to_single_json(string_of_json)(json))^"]"
    | Object (json) -> "{"^concat_with(", ", mult_to_single_json(string_of_jsonobj)(json))^"}"

(* 5 *)
let rec take (n,xs) = 
  match xs with
    [] -> []
    | hd::tl -> if(n = 1) then [hd] else [hd]@take(n-1, tl)

(* 6 *)
let rec firsts xs = 
  match xs with 
    [] -> []
    | (k,v)::tl -> [k]@firsts(tl)

(* 7 *)
(*
Suppose xs has type (int * int) list, and let n be an integer between 0 and the length of xs (inclusive),
and consider the expressions firsts (take (n, xs)) and take (n, firsts xs). Either (1) write one
sentence explaining in informal but precise English why these two expressions always evaluate to the
same value; or (2) give example values of xs and n such that the two expressions evaluate to different
values. Regardless of whether you decide option (1) or option (2) is correct, also write one sentence
explaining which of the two expressions above might be faster to evaluate and why.

These will always return the same value because let us say we have an integer n and a list of pairs xs.
If we call firsts(take(n, xs)) we will get firsts of the first n pairs in xs, which will in turn give us 
the first n first-components in xs.
If we call take(n, firsts(xs)) weill get take of n, the first-components of xs. This will then give us
the first n first-components in xs, which is the same as we saw of firsts(take(n, xs)).
Since the order is preserved for both take and firsts, we will always get the same result.

*)

(* 8 *)
let rec assoc (k, xs) =
  match xs with
    [] -> None 
    | (key,v)::tl -> if(key = k) then Some(v) else assoc(k, tl)

(* 9 *)
let dot (j, f) = 
  match j with
    Object(j) -> assoc(f, j)
    | _ -> None
  
(* 10 *)
let rec dots (j, fs) =
  match fs with 
    [] -> None
    | hd :: [] -> dot(j, hd)
    | (hd::tl) -> if(dot(j, hd) = None) then None else dots(some_to_item(dot(j, hd)), tl)

(* 11 *)
let one_fields j =
  let rec tail_recurse_for_fields(j, keys) = 
    match j with
      [] -> keys
      | (k,v)::tl -> tail_recurse_for_fields(tl, k::keys)
  in
  match j with
    Object(j) -> tail_recurse_for_fields(j, [])
    | _ -> []

(* 12 *)
let no_repeats xs = 
  if(List.length xs = List.length(dedup(xs))) then true else false

(* 13 *)
let rec recursive_no_field_repeats j =
  let rec process_json_arr(arr) = 
    match arr with 
      [] -> []
      | hd::tl -> hd@process_json_arr(tl)
  in
  let rec allkeys(j) = 
    match j with
    | Object [] -> []
    | Object ((k, v)::tl) -> [k]@allkeys(v)@allkeys((Object tl))
    | Array (sub_objs) -> (process_json_arr (mult_to_single_json allkeys sub_objs))
    | _ -> []
  in
  no_repeats(allkeys(j))

let rec count_occur(xs, count, e) = 
  match xs with
  | [] -> []
  | h::[] -> [(h, count+1)]
  | h::h2::t -> if(h>h2) then raise(e)
      else 
        if (h = h2) then count_occur(h2::t, count+1, e)
        else [(h, count+1)]@count_occur(h2::t, 0, e)

(* 14 *)
let count_occurrences (xs, e) =
  count_occur(xs, 0, e)

(* 15 *)
let string_list_from_val (j) =
    match j with
    | Some String (s) -> [ s ]
    | _ -> []

let rec string_values_for_access_path (fs, js) = 
  match js with 
    [] -> []
    | hd::tl -> string_list_from_val(dots(hd, fs))@string_values_for_access_path(fs, tl)

let rec filter_access_path (fs, v, js) = 
  match js with 
    [] -> []
    | hd::tl -> if(Some(String(v)) = dots(hd, fs) ) then [hd]@filter_access_path(fs, v, tl) else filter_access_path(fs, v, tl)

(* 16 *)
let rec filter_access_path_value (fs, v, js) = 
  filter_access_path(fs, v, js)

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(* 17 *)
let in_rect (r, p) = 
  if p.latitude > r.min_latitude && p.latitude < r.max_latitude 
  && p.longitude > r.min_longitude && p.longitude < r.max_longitude then true
  else false

let jsonnum(j, f) =
    match (dot (j, f)) with
    | Some Num (num) -> (Some num)
    | _ -> None

(* 18 *)
let point_of_json j = 
  let lat = jsonnum(j, "latitude") in 
  let long = jsonnum(j, "longitude") in 
  if (lat = None || long = None) then None 
  else Some { latitude = some_to_item(lat); longitude = some_to_item(long)}

let rec filter_access_path_rect (fs, r, js) = 
  match js with 
    [] -> []
    | hd::tl -> if(dots(hd, fs) = None) then filter_access_path_rect(fs, r, tl)
     else if(in_rect(r, some_to_item(point_of_json(some_to_item(dots(hd, fs))))) = false) then filter_access_path_rect(fs, r, tl)
     else [hd]@filter_access_path_rect(fs, r, tl)

(* 19 *)
let rec filter_access_path_in_rect (fs, r, js) = 
  filter_access_path_rect(fs, r, js)

(* 20 *)
(* write your comment here *)

(* The definition of the U district for purposes of this assignment :) *)
let u_district =
  { min_latitude  =  47.648637;
    min_longitude = -122.322099;
    max_latitude  =  47.661176;
    max_longitude = -122.301019
  }

(* For this section, we provide the definition of U district and the functions
 * to calculate a histogram. Use these to create the bindings as requested. 
 * But notice our implementation of histogram uses *your* definition of count_occurrences
 *)
 (* We provide this code commented out because it uses some of your functions 
    that you haven't implemented yet *)

(*
exception SortIsBroken

(* Creates a histogram for the given list of strings. 
 * Returns a tuple in which the first element is
 * a string, and the second is the number of times that string
 * is found. *)
let histogram xs = 
  let sorted_xs = List.sort (fun a b -> compare a b) xs in
  let counts = count_occurrences (sorted_xs, SortIsBroken) in
  let compare_counts (s1, n1) (s2, n2) =
    if n1 = n2 then compare s1 s2 else compare n1 n2
  in
  List.rev (List.sort compare_counts counts)

let histogram_for_access_path (fs, js) = 
  histogram (string_values_for_access_path (fs,js))

(* notice we use *your* definition of dot *)
let complete_bus_positions_list =
  match (dot (complete_bus_positions, "entity")) with
  | Some (Array xs) -> xs
  | _ -> failwith "complete_bus_positions_list"

*)
exception Unimplemented
let route_histogram     = Unimplemented
let top_three_routes    = Unimplemented
let buses_in_ud         = Unimplemented
let ud_route_histogram  = Unimplemented
let top_three_ud_routes = Unimplemented
let all_fourty_fours    = Unimplemented
