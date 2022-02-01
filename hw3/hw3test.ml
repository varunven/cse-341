(* NOTE: Uncomment the following line if you want to #use this file in utop
 * (optionally, call it from utop directly): *)
 #mod_use "hw3types.ml";; 

open Hw3types


(* #1 *)
let strs = ["A"; "B"; "c"; "chimichanga"; "QuesadILLA"; "thisisthelongestword1"; "thisisthelongestword2"; "NOTLONG"; "%"]

let only_lowercase_test = only_lowercase(strs) = ["c"; "chimichanga"; "thisisthelongestword1"; "thisisthelongestword2"; "%"]

(* #2 *)
let longest_string1_test = longest_string1(strs) = "thisisthelongestword1"

(* #3 *)
let longest_string2_test = longest_string2(strs) = "thisisthelongestword2"

(* #4 *)
let longest_string3_test = longest_string3(strs) = "thisisthelongestword1"

let longest_string4_test = longest_string4(strs) = "thisisthelongestword2"

(* #5 *)
let longest_lowercase_test = longest_lowercase(strs) = "thisisthelongestword1"

(* #6 *)
let str = "aBxXXxDdx"
let caps_no_X_string_test = caps_no_X_string(str) = "ABDD"

(* #7 *)
let first_answer_test = 
    (first_answer (fun x -> if (String.length x > 1) then Some x else None) strs) = "chimichanga"
(* let first_answer_testb = (first_answer (fun x -> if (x mod 2 = 0) then Some x else None) []) *)

(* #8 *)
let all_answers_test =
    (all_answers (fun x -> if (x mod 2 = 0) then (Some [x;x]) else None) [2;4]) = (Some [2;2;4;4])

(* #9 *)
let count_wildcards_test =
    count_wildcards(ConstructorP("foo", TupleP [WildcardP; ConstantP 1])) = 1

let count_wild_and_variable_lengths_test =
    count_wild_and_variable_lengths(ConstructorP("foo", TupleP [WildcardP; VariableP "xyz"])) = 4

let count_a_var_test = 
    ((count_a_var "xyz" (ConstructorP("foo", TupleP [WildcardP; VariableP "xyz"]))) = 1)
    && ((count_a_var "foo" (ConstructorP("foo", TupleP [WildcardP; VariableP "xyz"]))) = 0)

(* #10 *)
let check_pat_test = 
    (check_pat(TupleP [
        ConstructorP("foo", WildcardP);
        ConstructorP("bar", ConstantP(1));
        TupleP [
            ConstructorP("xyz", WildcardP);
            ConstructorP("t", ConstantP(1));
        ];
    ]) = true) && 
        (check_pat(TupleP [
        ConstructorP("foo", WildcardP);
        ConstructorP("xyz", ConstantP(1));
        TupleP [
            ConstructorP("xyz", WildcardP);
            ConstructorP("t", ConstantP(1));
        ];
    ]) = false)

(* #11 *)
let rec matches_test = 
    (matches 
        ( Tuple [(Constant 1); Unit] )
        ( TupleP [(VariableP "foo"); UnitP] )) = Some [("foo", Constant 1)]

(* #12 *)
let first_match_test = 
  (first_match (Constant 1) [UnitP; (ConstantP 1)]) = Some []
  && (first_match (Constant 1) [UnitP; (ConstantP 2)]) = None

(* optional challenge problem  *)

let typecheck_patterns_test = "Did not implement"
