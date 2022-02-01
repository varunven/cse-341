(* CSE 341, Spring 2021, Midterm Quiz Sample Solution *)

type person_in_zoom = string * bool * bool (* name, video off?, muted? *)

type nested_zoom =
| Room of person_in_zoom list
| Breakouts of nested_zoom list 

(* Q2 *)
let example_no_breakouts = Room [("Dan",false,false);("Kenny",true,true);("Abdul",true,true)]

let example_full_breakouts = Breakouts [Room[("Dan",false,false)];Room[("Kenny",true,true)];Room[("Abdul",true,true)]]

(* Q3 *)

(* mystery1 computes the number of people in the zoom room, adding up the number of people in all nested breakouts *)

(* mystery2 returns a list of all the people in the zoom room, including in any nested breakouts, with the list containing
  the triples of their name, video status, and mute status
*)

(* Q4 *)

type better_answer = Muted | NotMuted | NoSuchPerson
exception RepeatedPerson
let merge pr =
  match pr with
  | NoSuchPerson, NoSuchPerson -> NoSuchPerson (* 1 *)
  | x,NoSuchPerson -> x                        (* 2 *)
  | NoSuchPerson,x -> x                        (* 3 *)
  | _ -> raise RepeatedPerson                  (* 4 *)

let rec is_muted2 s nz =
  let rec loop_people ps =
    match ps with
    | [] -> NoSuchPerson
    | (s',_,m)::ps' -> merge ((if s<>s' then NoSuchPerson else if m then Muted else NotMuted), (* I don't expect this solution; longer is fine *)
                              loop_people ps') in 
  let rec loop_breakouts nzs =
    match nzs with
    | [] -> NoSuchPerson
    | nz'::nzs' -> merge (is_muted2 s nz', loop_breakouts nzs') in
  match nz with
  | Room ps -> loop_people ps
  | Breakouts nzs -> loop_breakouts nzs

(* Q5 *)

(* No, they are not equivalent.  For any nested_zoom that has multiple people named Dan, g1
   will not raise an exception and g2 will raise an exception.  For example:
*)
let x : nested_zoom = Room[("Dan",true,true);("Dan",true,true)]

(* Q6 *)

let rec zoom_map f nzs =
  match nzs with 
  | Room ps -> Room (List.map f ps)
  | Breakouts nzs -> Breakouts (List.map (zoom_map f) nzs)

let mute_all = zoom_map (fun (s,v,_) -> (s,v,true))

let mute_all_except s = zoom_map (fun (s',v,_) -> if s=s' then (s',v,false) else (s,v,true))

(* Q7 *)

(*
 7.1 equivalent
 7.2 neither
 7.3 neither
 7.4 inexhaustive pattern match
*)

(* Q8 *)

(*
 8.1: yes
 8.2: no
 8.3: no
 8.4: yes
 8.5: yes
 8.6: no
*)

(* Q9 *)

module type COLOR = sig
type color
val red : color
val green : color
val blue : color
val brighten : color -> color
val darken : color -> color
val add : color -> color -> color

val is_white : color -> bool
end

module Color : COLOR = struct
type color = int * int * int (* (red value, green value, blue value) *)
let red = (255,0,0)
let green = (0,255,0)
let blue = (0,0,255)

let purple = (127,0,127) (* private *)
let white = (255,255,255) (* private *)

let is_white (r,g,b) = r >= 255 && g >= 255 && b >= 255
(* here are 3 solutions to part (c) *)
let is_white1 c = (c = white)
let is_white2 (r,g,b) = r = 255 && g = 255 && b = 255
let is_white3 (r,g,b) = (r+g+b) = 765
(* Note it is wrong to do r > 254 && g > 254 && b > 254 as w/o an abstract type clients still couldn't tell *)
let max255 i = if i > 255 then 255 else i (* private *)

let darken (r,g,b) = (r/2, g/2, b/2) 

let brighten (r,g,b) = (max255 (r*2), max255 (g*2), max255 (b * 2))

let add (r1,g1,b1) (r2,g2,b2) = (max255 (r1+r2), max255 (g1+g2), max255(b1+b2))

end

let purple = Color.darken (Color.add Color.red Color.blue)