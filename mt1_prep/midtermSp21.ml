type person_in_zoom = string * bool * bool (* name, video off?, muted? *)

type nested_zoom =
| Room of person_in_zoom list
| Breakouts of nested_zoom list 

(* mystery1 computes the total number of people there are in all of the nested rooms in the breakout
 room passed in *)
let rec mystery1 nz =
  match nz with
  | Room ps -> List.length ps
  | Breakouts nzs -> List.fold_left (fun acc nz -> acc + mystery1 nz) 0 nzs

(* mystery2 returns alist of every person in every room in the nested breakouts *)
let rec mystery2 nz =
  match nz with
  | Room ps -> ps
  | Breakouts nzs -> List.fold_left (fun acc nz' -> mystery2 nz' @ acc) [] nzs

(* Returns if a person if certain named is muted or not *)
let rec is_muted s nz =
(* Loop through all ppl, if found person w same name then return muted as var *)
  let rec loop_people ps =
    match ps with
    | [] -> None
    | (s',_,m)::ps' -> if s = s' then Some m else loop_people ps' in
(* Loop through breakouts and call is_muted on inner nested_zooms and continue until found muted/not*)
  let rec loop_breakouts nzs =
    match nzs with
    | [] -> None
    | nz'::nzs' -> (match is_muted s nz' with 
                      | Some x -> Some x 
                      | None -> loop_breakouts nzs') in
(* If Room, loop_people, if breakout, loop_breakout *)
  match nz with
  | Room ps -> loop_people ps
  | Breakouts nzs -> loop_breakouts nzs

type better_answer = Muted | NotMuted | NoSuchPerson

exception RepeatedPerson

(* Match a tuple with nosuchperson and return if nosuchperson, certainperson, or repeatedperson *)
let merge pr =
  match pr with
  | (NoSuchPerson, NoSuchPerson) -> NoSuchPerson 
  | (x, NoSuchPerson) -> x                        
  | (NoSuchPerson, x) -> x                        
  | _ -> raise RepeatedPerson

let dan = ("Dan", false, false)
let ken = ("Kenny", true, true) 
let abdul = ("Abdul", true, true)

let twoa = Room [dan; ken; abdul]
let twob = Breakouts[Room[dan]; Room[ken]; Room[abdul]]

let m1a = mystery1 twoa 
let m1b = mystery1 twob

let m2a = mystery2 twoa 
let m2b = mystery2 twob

(* Returns if a person if certain named is muted or not form better_answer and uses merge*)
let rec is_muted2 s nz =
(* Loop through all ppl, if found person w same name then return muted as var *)
  let rec loop_room ps =
    match ps with
    | [] -> NoSuchPerson
    | (s',_,m)::ps' -> if s = s' then if(m) then merge(Muted, loop_room ps') else merge(NotMuted, loop_room ps') else merge(NoSuchPerson, loop_room ps') in
(* Loop through breakouts and call is_muted on inner nested_zooms and continue until found muted/not*)
  let rec loop_breakouts nzs =
    match nzs with
    | [] -> NoSuchPerson
    | nz'::nzs' -> merge(is_muted2 s nz', loop_breakouts nzs') in
(* If Room, loop_people, if breakout, loop_breakout *)
  match nz with
  | Room ps -> loop_room ps
  | Breakouts nzs -> loop_breakouts nzs


let mu1a = is_muted "Name" twoa
let mu1b = is_muted "Name" twob
let mu1c = is_muted "Dan" twoa
let mu1d = is_muted "Abdul" twob

let mu2a = is_muted2 "Name" twoa
let mu2b = is_muted2 "Name" twob
let mu2c = is_muted2 "Dan" twoa
let mu2d = is_muted2 "Abdul" twob

(* Not the same because g2 does not have a mapping for if the return option is RepeatedPerson exception *)
let g1 nz = is_muted "Dan" nz
let g2 nz = match is_muted2 "Dan" nz with 
            | NoSuchPerson -> None 
            | Muted -> Some true 
            | NotMuted -> Some false

let rec zoom_map f xs = 
  match xs with 
    Room r -> Room (List.map f r)
    | Breakouts b -> Breakouts (List.map(zoom_map f) b)

let mute_all xs = zoom_map (fun (s,v,m)->(s,v,true)) xs 

let mute_all_except s xs = zoom_map (fun (s',v,m)->if(s=s') then (s,v,false) else (s,v,true)) xs

(* 7: Same, Not Same, Not Same, Inexhaustive *)

(* 8: Y, N, N, Y, Y, N *)

module type COLOR = sig
  type color
  val red : color
  val green : color
  val blue : color
 
  val is_white : color -> bool
 
  val max255 : int -> int
 
  val darken : color -> color 
 
  val brighten : color -> color
 
  val add : color -> color -> color
end
 
module Color : COLOR = struct
  type color = int * int * int (* (red value, green value, blue value) *)
  let red = (255,0,0)
  let green = (0,255,0)
  let blue = (0,0,255)
 
  let purple = (127,0,127) (* private *)
  let white = (255,255,255) (* private *)
 
  let is_white (r,g,b) = r >= 255 && g >= 255 && b >= 255 (* see part (c) *)
  let is_white (r,g,b) = (r+g+b = 765)
  let max255 i = if i > 255 then 255 else i (* private *)
 
  let darken (r,g,b) = (r/2, g/2, b/2) 
 
  let brighten (r,g,b) = (max255 (r*2), max255 (g*2), max255 (b * 2))
 
  let add (r1,g1,b1) (r2,g2,b2) = (max255 (r1+r2), max255 (g1+g2), max255(b1+b2))
 
end
 
let purple = Color.darken(Color.add Color.red Color.blue)
