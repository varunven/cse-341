module type STRINGMULTISET = sig
  type t 
  val empty: t
  val insert: string -> t -> t
  val remove: string -> t -> t
  val count: string -> t -> int
end

module StringMultiset1 : STRINGMULTISET = struct
  type t = string list

  let empty = []

  let insert s xs = s::xs

  let rec remove s xs =
    match xs with
    | [] -> []
    | x::xs' -> if s=x then xs' else x :: remove s xs'

  let rec count s xs =
    match xs with
    | [] -> 0
    | x::xs' -> (if s=x then 1 else 0) + count s xs'
end

module StringMultiset2 : STRINGMULTISET = struct
  type t = (string * int) list

  let empty = []

  let rec insert s xs =
    match xs with
    | [] -> [(s,1)]
    | (s',c)::xs' -> if s=s' then (s,c+1)::xs' else (s',c) :: insert s xs'

  let rec remove s xs =
    match xs with
    | [] -> []
    | (s',c)::xs' -> if s=s' then 
                       (if c=1 then xs' else (s,c-1)::xs') 
                     else 
                       (s',c) :: remove s xs'

  let rec count s xs =
    match xs with
    | [] -> 0
    | (s',c)::xs' -> if s=s' then c else count s xs'
end