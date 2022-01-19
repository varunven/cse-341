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

let fstpair(a, _) = a
let sndpair(_, b) = b

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
  
(* 4 *)
let rec string_of_json j =
  failwith "Need to implement: string_of_json"

(* 5 *)
let rec take (n,xs) = 
  match xs with
    [] -> []
    | hd::tl -> if(n = 1) then [hd] else [hd]@take(n-1, tl)

(* 6 *)
let rec firsts xs = 
  match xs with 
    [] -> []
    | hd::tl -> [fstpair(hd)]@firsts(tl)

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
    | hd::tl -> if(fstpair(hd) = k) then Some(sndpair(hd)) else assoc(k, tl)

(* 9 *)
let dot (j, f) = 
  
  failwith "Need to implement: dot"

(* 10 *)
let rec dots (j, fs) =
  failwith "Need to implement: dots"

(* 11 *)
let one_fields j =
  failwith "Need to implement: one_fields"

(* 12 *)
let no_repeats xs = 
  failwith "Need to implement: no_repeats"

(* 13 *)
let rec recursive_no_field_repeats j = 
  failwith "Need to implement: recursive_no_field_repeats"

(* 14 *)
let count_occurrences (xs, e) =
  failwith "Need to implement: count_occurrences"

(* 15 *)
let rec string_values_for_access_path (fs, js) = 
  failwith "Need to implement: string_values_for_access_path"

(* 16 *)
let rec filter_access_path_value (fs, v, js) = 
  failwith "Need to implement: filter_access_path_value"

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(* 17 *)
let in_rect (r, p) = 
  failwith "Need to implement: in_rect"

(* 18 *)
let point_of_json j = 
  failwith "Need to implement: point_of_json"

(* 19 *)
let rec filter_access_path_in_rect (fs, r, js) = 
  failwith "Need to implement: filter_access_path_in_rect"

(* 20 *)
(* write your comment here *)

(* For this section, we provide the definition of U district and the functions
 * to calculate a histogram. Use these to create the bindings as requested. 
 * But notice our implementation of histogram uses *your* definition of count_occurrences
 *)
 (* We provide this code commented out because it uses some of your functions 
    that you haven't implemented yet *)

(*
exception SortIsBroken

(* The definition of the U district for purposes of this assignment :) *)
let u_district =
  { min_latitude  =  47.648637;
    min_longitude = -122.322099;
    max_latitude  =  47.661176;
    max_longitude = -122.301019
  }

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
