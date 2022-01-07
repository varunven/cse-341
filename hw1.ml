(* CSE 341, Homework 1, Provided Code *)

(* You might choose to uncomment these, like the lecture code does *)
(* #utop_prompt_dummy
   let _ = UTop.set_show_box false *)

(* Use these functions to extract parts of a date *)
let fst3 (x,_,_) = x (* gets the first element of a triple *)
let snd3 (_,x,_) = x (* gets the second element of a triple *)
let thd3 (_,_,x) = x (* gets the third element of a triple *)

(* Date structure is: day, month, year
 *)

 (* 1 *)
 (* Use series of conditionals to check if ever a tie, otherwise compare and return T/F correspondingly *)
let is_older ((date1 : int * int * int), (date2 : int * int * int)) = 
  if thd3(date1) = thd3(date2) then (
    if snd3(date1) = snd3(date2) then (
        if fst3(date1) < fst3(date2) then true
        else false)
    else if snd3(date1) < snd3(date2) then true
    else false)
  else if thd3(date1) < thd3(date2) then true
  else false
    
(* 2 *)
(*iterate through list of dates, check if date second elem equals month, if so increment, return end count*)
let rec number_in_month ((dates : (int * int * int) list), (month : int)) =
  if dates = [] then 0
  else (if snd3(List.hd dates) = month then 1 + number_in_month(List.tl dates, month) else number_in_month(List.tl dates, month))

(* 3 *)
(*iterate through list of months, increment by output of number_in_month given list of dates and curr month, return end count*)
let rec number_in_months ((dates : (int * int * int) list), (months : (int) list)) =
    if months = [] then 0
    else number_in_month(dates, List.hd months) + number_in_months(dates, List.tl months)

(* 4 *)
(*iterate through dates list, check if date second elem equals month, if so add to list, return list*)
let rec dates_in_month ((dates : (int * int * int) list), (month : int)) =
  if dates = [] then []
  else (if snd3(List.hd dates) = month then List.hd dates :: dates_in_month(List.tl dates, month) else dates_in_month(List.tl dates, month))

(* 5 *)
(*iterate through months list, increment by output of number_in_month with dates list and month, return count*)
let rec dates_in_months ((dates : (int * int * int) list), (months : (int) list)) =
    if months = [] then []
    else dates_in_month(dates, List.hd months)@dates_in_months(dates, List.tl months)

(* 6 *)
(*iterate through list, each time decrement n and return element when n=0*)
let rec get_nth ((items : (string) list), (n : int)) =
  if n = 0 then List.hd items
  else get_nth(List.tl items, n-1)

(* 7 *)
(* Declare list of months as strings, use to identify each int month as a string and concatenate *)
let months = ["January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"]
let string_of_date (date : (int * int * int)) =
  (get_nth(months, snd3(date)-1)^"-"^string_of_int(fst3(date))^"-"^string_of_int(thd3(date)))

(* 8 *)
let rec _goThrough((i: int), (stop : int), (max : int), (vals : (int) list)) = 
  if vals = [] then 0
  else (if (stop + List.hd vals >= max) then i - 1 else _goThrough(i+1, stop + List.hd vals, max, List.tl vals))

(* Iterate through list recursively, each time subtracting val at index from sum, break when sum <= 0 *)
let number_before_reaching_sum((sum : int), (nums : (int) list)) = 
  _goThrough(1, 0, sum, nums)

(* 9 *)
let daysinmonths = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]
(* Pass daysinmonths and given day into number_before_reaching_sum and get result +1  *)
let what_month((day : int)) = 
  (1 + number_before_reaching_sum(daysinmonths, day))

(* 10 *)
(* Helper function that iterates through two nums and returns a list of the nums inbetween *)
let rec _getBetween((month1 : int), (month2 : int)) = 
  if month1 = month2 then [month2]
  else [month1]@_getBetween(month1+1, month2)

(* If day1 > day2, then empty list, otherwise equal to result of _getBetween(day1month, day2month) *)
let month_range((day1 : int), (day2 : int)) =
  if day1 > day2 then []
  else 
    _getBetween(what_month(day1), what_month(day2))

(* 11 *)
(* Helper function that iterates through dates and returns max date *)
let rec _getOldest((dates : (int * int * int) list)) = 
  if(List.tl dates = []) then List.hd dates
  else (if(is_older(List.hd dates, _getOldest(List.tl dates))) then List.hd dates else _getOldest(List.tl dates))

(* If empty then None, otherwise use helper method to find oldest *)
let oldest((dates : (int * int * int) list)) = 
    if dates = [] then None
    else Some (_getOldest(dates))

(* 12 *)
(* Helper function that iterates through nums and returns partial sums *)
let rec get_cum_sum((nums : (int) list), (sum: int)) = 
  if nums = [] then []
  else [sum+List.hd nums]@get_cum_sum(List.tl nums, sum+List.hd nums)

let cumulative_sum((nums : (int) list)) = 
  get_cum_sum(nums, 0)