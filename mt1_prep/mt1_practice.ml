type person_in_zoom = string * bool * bool (* name, video off?, muted? *)

type nested_zoom =
| Room of person_in_zoom list
| Breakouts of nested_zoom list 

let rec is_muted s nz =
  let rec loop_people ps =
    match ps with
    | [] -> None
    | (s',_,m)::ps' -> if s = s' then Some m else loop_people ps' in
  let rec loop_breakouts nzs =
    match nzs with
    | [] -> None
    | nz'::nzs' -> (match is_muted s nz' with 
                      | Some x -> Some x 
                      | None -> loop_breakouts nzs') in
  match nz with
  | Room ps -> loop_people ps
  | Breakouts nzs -> loop_breakouts nzs

type better_answer = Muted | NotMuted | NoSuchPerson

exception RepeatedPerson

let merge pr =
  match pr with
  | (NoSuchPerson, NoSuchPerson) -> NoSuchPerson 
  | (x, NoSuchPerson) -> x                        
  | (NoSuchPerson, x) -> x                        
  | _ -> raise RepeatedPerson

let dan = "Dan" * false * false
let ken = ("Kenny" * true * true) 
let abdul = ("Abdul" * true * true)

