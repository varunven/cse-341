(* CSE 341, Homework 1, Provided Code *)

(* You might choose to uncomment these, like the lecture code does *)
(* #utop_prompt_dummy
   let _ = UTop.set_show_box false *)

(* Use these functions to extract parts of a date *)

let fst3 (x,_,_) = x (* gets the first element of a triple *)
let snd3 (_,x,_) = x (* gets the second element of a triple *)
let thd3 (_,_,x) = x (* gets the third element of a triple *)

(* Test dates to use in the following test methods*)
let date1 = (3, 3, 3)
let date2 = (1, 12, 4)
let date3 = (16, 13, 9)
let date4 = (29, 3, 8)
let date5 = (31, 4, 6)
let date6 = (7, 8, 1)

let d1 = [date1; date3; date5; date6]
let d2 = [date1; date2; date3; date4; date6]
let d3 = [date1; date3; date5]
let d4 = [date2; date3; date4]

let month1 = 6
let month2 = 3
let month3 = 2
let month4 = 12
let month5 = 8
let month6 = 4

let m1 = [month1; month2; month3; month5]
let m2 = [month2; month4; month5; month6]

let s = ["aggy"; "baggy"; "haggy"; "waggy"; "laggy"; "raggy"]

let n = [1; 4; 2; 7; 9; 13; 16; 0; 1; 4; 34]
let n2 = [3; 8; 13; 0; 1; 2]
let n3 = [14; 2; 3; 1; 6; 0; 0]

(*Method 1 tests*)
let m1test1 = is_older(date1, date2) (* should be true*)
let m1test2 = is_older(date2, date1) (* should be false *)
let m1test3 = is_older(date1, date1) (* should be false *)
let m1test4 = is_older(date1, date4) (* should be true *)
let m1test5 = is_older(date1, date3) (* should be true *)
let m1test6 = is_older(date4, date2) (* should be false *)

(*Method 2 tests*)
let m2test1 = number_in_month(d1, 1) (* should be 0*)
let m2test2 = number_in_month(d2, 4) (* should be 0 *)
let m2test3 = number_in_month(d1, 8) (* should be 1 *)
let m2test4 = number_in_month(d1, 3) (* should be 1 *)
let m2test5 = number_in_month(d2, 3) (* should be 2 *)

(*Method 3 tests*)
let m3test1 = number_in_months(d1, m1) (* should be 2 *)
let m3test2 = number_in_months(d2, m1) (* should be 3 *)
let m3test3 = number_in_months(d1, m2) (* should be 3 *)
let m3test4 = number_in_months(d2, m2) (* should be 4 *)

(*Method 4 tests*)
let m4test1 = dates_in_month(d1, 1) (* should be []*)
let m4test2 = dates_in_month(d2, 4) (* should be [] *)
let m4test3 = dates_in_month(d1, 8) (* should be [7,8,1] *)
let m4test4 = dates_in_month(d1, 3) (* should be [3,3,3] *)
let m4test5 = dates_in_month(d2, 3) (* should be [3,3,3; 29,3,8] *)

(*Method 5 tests*)
let m5test1 = dates_in_months(d1, m1) (* should be [3,3,3; 7,8,1] *)
let m5test2 = dates_in_months(d2, m1) (* should be [3,3,3; 29,3,8; 7,8,1] *)
let m5test3 = dates_in_months(d1, m2) (* should be [3,3,3; 7,8,1; 31,4,6] *)
let m5test4 = dates_in_months(d2, m2) (* should be [3,3,3; 29,3,8; 1,12,4; 7,8,1] *)

(*Method 6 tests*)
let m6test1 = get_nth(s, 3) (* should be waggy *)
let m6test2 = get_nth(s, 2) (* should be haggy *)
let m6test3 = get_nth(s, 5) (* should be raggy *)

(*Method 7 tests*)

let m7test1 = string_of_date(date1) (* should be March-3-3 *)
let m7test2 = string_of_date(date2) (* should be December-1-4 *)
let m7test3 = string_of_date(date5) (* should be April-31-6 *)
let m7test4 = string_of_date(date6) (* should be August-7-1 *)

(*Method 8 tests*)

let m8test1 = number_before_reaching_sum(n, 18) (* should be 4 *)
let m8test2 = number_before_reaching_sum(n, 62) (* should be 10 *)
let m8test3 = number_before_reaching_sum(n, 6) (* should be 2 *)
let m8test4 = number_before_reaching_sum(n, 7) (* should be 2 *)
let m8test5 = number_before_reaching_sum(n, 8) (* should be 3 *)
let m8test6 = number_before_reaching_sum(n, 32) (* should be 5 *)

(*Method 9 tests*)

let m9test1 = what_month(144) (* should be 5 *)
let m9test2 = what_month(362) (* should be 12 *)
let m9test3 = what_month(33) (* should be 2 *)
let m9test4 = what_month(60) (* should be 3 *)

(*Method 10 tests*)

let m10test1 = month_range(31, 86) (* should be [1; 2; 3]*)
let m10test2 = month_range(183, 344) (* should be [7; 8; 9; 10; 11; 12] *)
let m10test3 = month_range(1, 18) (* should be [1] *)
let m10test4 = month_range(71, 266) (* should be [3; 4; 5; 6; 7; 8; 9]*)
let m10test4 = month_range(30, 60) (* should be [1; 2; 3] *)

(*Method 11 tests*)
let m11test1 = oldest(d1) (* should be (7,8,1) *)
let m11test2 = oldest(d2) (* should be (7,8,1) *)
let m11test3 = oldest(d3) (* should be (3,3,3) *)
let m11test4 = oldest(d4) (* should be (1,12,4) *)

(*Method 12 tests*)

let m12test1 = cumulative_sum(n) (* should be [1; 5; 7; 14; 23; 36; 52; 52; 53; 57; 91]*)
let m12test2 = cumulative_sum(n2) (* should be [3; 11; 24; 24; 25; 27] *)
let m12test3 = cumulative_sum(n3) (* should be [14; 16; 19; 20; 26; 26; 26] *)