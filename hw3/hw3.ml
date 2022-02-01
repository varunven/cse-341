(* NOTE: Uncomment the following line if you want to #use this file in utop
 * (optionally, call it from utop directly): *)
#mod_use "hw3types.ml";;

(* NOTE: to get rid off the red-wiggles in VSCode, first compile the
 * the hw3types module running this 
 * from the command line: 
       ocamlopt hw3types.ml
 *)
open Hw3types

(**** Implement the following functions, remembering the "important note on function bindings" in the assignment write-up ****)

(* #1 *)
let only_lowercase =
  List.filter (fun str -> str.[0] == Char.lowercase_ascii(str.[0]))

(* #2 *)
let longest_string1 =
  List.fold_left (fun x y -> if(String.length(x) >= String.length(y)) then x else y) ""

(* #3 *)
let longest_string2 =
  List.fold_left (fun x y -> if(String.length(x) > String.length(y)) then x else y) ""

(* #4 *)
let longest_string_helper f =
    List.fold_left (fun x y -> if (f (String.length x) (String.length y)) then x else y) ""

let longest_string3 = longest_string_helper (>=)

let longest_string4 = longest_string_helper (>)
  
(* #5 *)
let longest_lowercase = longest_string1%only_lowercase

(* #6 *)
let caps_no_X_string = (String.concat "") % (String.split_on_char 'X') % String.uppercase_ascii

(* #7 *)
let option_is_valid item = 
  match item with 
    Some(item) -> true
    | _ -> false

let option_get item = 
  match item with 
    Some(item) -> item
    | _ -> assert(false)
  
let rec first_answer f xs = 
  match (List.map f xs) |> List.filter (option_is_valid) with
  | hd :: tl -> (option_get hd)
  | [] -> raise NoAnswer

(* #8 *)
let all_answers f xs = 
    List.fold_left
        (fun x y ->
            if (option_is_valid y) && (option_is_valid x) then
                (Some ((option_get x) @ (option_get y)))
            else
                None
        )
        (Some [])
        (List.map f xs)

(* #9 *)
(*
  count_wildcards accepts two functions, f1 and f2, and a pattern p
  It returns the nested loop of applying the pattern to both f1 and f2
  If p contains another pattern within it, that is also applied to the same two functions... 
  ...and added to the result
*)
let count_wildcards =
   g (fun () -> 1) (fun _ -> 0)

let count_wild_and_variable_lengths =
  g (fun () -> 1) (fun str -> (String.length str))

let count_a_var s = 
  g (fun () -> 0) (fun str -> if str = s then 1 else 0)

(* #10 *)
let check_pat pat = 
    let rec get_vars (p : pattern) (vs : string list) =
        match p with
        | ConstructorP(v, p) -> (get_vars p (v :: vs))
        | TupleP (ps) -> List.fold_left (fun a b -> (a @ (get_vars b []))) vs ps
        | _ -> vs
    in
    let no_repeats (xs) =
        (List.length xs) = (List.length (List.sort_uniq compare xs))
    in
    let vars = (get_vars pat []) in
    (no_repeats vars)

(* #11 *)
let rec matches v pat = 
    match v, pat with
    | Constructor(ka, va), ConstructorP(kb, vb) ->
            if ka = kb then (matches va vb) else None
    | Tuple (ta), TupleP(tb) ->
            if (List.length ta) != (List.length tb) then
                None
            else
                (all_answers (fun (v, p) -> (matches v p)) (List.combine ta tb))
    | Constant(v), ConstantP(p) -> if v = p then Some [] else None
    | Constant(v_value), VariableP(key) -> Some [(key, v)]
    | Unit, VariableP(key) -> Some [(key, Unit)]
    | Unit, UnitP -> Some []
    | _, WildcardP -> Some []
    | _, _ -> None

(* #12 *)
let first_match v pats = 
  try
      Some (first_answer (fun p -> (matches v p)) pats)
  with
  | NoAnswer -> None
  | _ -> assert(false)

(* optional challenge problem  *)

let typecheck_patterns cons pats = "This is optional"
