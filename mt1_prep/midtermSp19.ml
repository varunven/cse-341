(* 1 *)
type tree = I of int | S of string | N of tree * tree

(* 1a *)
let threeIntNoString = N(N(I(3), I(4)), I(5))

(* 1b *)
let rec sum t = 
    match t with 
        I i -> i 
        | S _ -> 0 
        | N(n, n2) -> sum n + sum n2

let y = sum(threeIntNoString)

(* 1c *)
let rec stringify t =  
    match t with 
        I i -> S(string_of_int(i))
        | S s -> S(s)
        | N(n, n2) -> N(stringify(n), stringify(n2))

let x = stringify(threeIntNoString)

(* 1d *)
(* let rec intify t = 
    match t with 
        I i -> I(i)
        | S s -> if(int_of_string(s) = None) then s else I(int_of_string(s))
        | N(n, n2) -> N(intify(n), intify(2)) *)

(* 2 *)
let rec has_exactly_two_ints t = (* this is buggy! *)
    match t with
    (* 1 *) | N(I _, I _) -> true
    (* 2 *) | N(t1, t2) -> has_exactly_two_ints t1 || has_exactly_two_ints t2
    (* 3 *) | S _ -> false
    (* 4 *) | I _ -> false

(* 2a *)
let isProperTrue = N(I(3), I(4))
let res = has_exactly_two_ints(isProperTrue)

(* 2b *)
let isImproperTrue = N(N(I(3), I(4)), I(5))
let res = has_exactly_two_ints(isImproperTrue)

(* 2c *)
let isProperFalse = I(3)
let res = has_exactly_two_ints(isProperFalse)

(* 2d *)
let isImproperFalse = N(N(I(3), S("X")), I(4))
let res = has_exactly_two_ints(isImproperFalse)

(* 2e
i: D
ii: A
iii: A
 *)

 (* 3 *)
(* 3a: y cannot be defined as x which is defined by g when g is defined by y
       g must be defined before it is used in x which is impossible *)
(* 3b: This does not check for all possible cases and is missing a return type when x!=y, y!=z, y<3 *)
(* 3c: Different return type between 0::[] from x+1 and (x+1)::(f3 xs) *)
(* 3d: y+0 has type int, but x has type float so cannot compare the two *)

(* 4 *)
let rec foo f g xs =
    match xs with
        [] -> []
        | x::xss ->
            let (y1,i1) = f x in
            let (y2,i2) = g x in
            (if i1 >= i2+1 then y1 else y2)::(foo f g xss)

(* 4a:
first func: returns 0, 2*x 
second func: returns x+1, x+1
[0;2;1;3]
f: [(0, 0);(0, 4);(0, 2);(0, 6)]
g: [(1, 1);(3, 3);(2, 2);(4, 4)]

[(1);(0);(2);(0)] 
[1;0;2;0]
*)

(* 4b:  ('a -> 'b * int) -> ('a -> 'b * int) -> 'a list -> 'b list *)

(* 4c *)
let foo f g = 
    List.map (fun x ->
            let (y1,i1) = f x in
            let (y2,i2) = g x in
            (if i1 >= i2+1 then y1 else y2)
    )

(* 4d:  (int -> int * int) -> (int -> int * int) -> int list -> int list *)

(* 4e: This is equivalent to a list of the absolute values of each number in the list *)

(* 5 *)
let rec filter2 f g xs =
    match xs with
        | [] -> []
        | x::xs -> if f x && g x
            then x :: (filter2 f g xs)
            else filter2 f g xs

(* 5a: TODO: find why N*)
(* 5b: Y*)
(* 5c: N*)
(* 5d: N*)
(* 5e *)
let filter2 f g = List.filter (fun x -> f x && g x)

(* 6 *)
module type DIRECTION = sig
type t
val turn : t * int -> t
val north : t
val isNS : t -> bool
val isEW : t -> bool
end

module Direction1 : DIRECTION = struct
    
    type t = North | East | South | West
    
    let turnClockwise x = match x with North -> East | East -> South | South -> West | West -> North
    
    let turnCounterClockwise x = match x with North -> West | West -> South | South -> East | East -> North
    
    let rec turn (x,n) =
        if n = 0
        then x
        else if n > 0
        then turn(turnClockwise x, n-1)
        else turn(turnCounterClockwise x, n+1)

    let north = North

    let isNS x =
        match x with
        | North -> true
        | South -> true
        | _ -> false

    let isEW x =
        match x with
        | East -> true
        | West -> true
        | _ -> false
end

module Direction2 : DIRECTION = struct
    type t = int (* 0 = North, 1 = East, 2 = South, 3 = West *)
    let turnClockwise x = if x=3 then 0 else x+1
    let turnCounterClockwise x = if x=0 then 3 else x-1
    let turn (x,n) = (x+n) mod 4
    let north = 0
    let isNS x = (x=0) || (x=2)
    let isEW x = (x=1) || (x=3)
end

(* 6a: B*)
(* 6b: TODO: find why A *)
(* 6c: 
i: TODO: find why impossible
ii: TODO: find why impossible
iii: Direction2.turn(Direction2.north, -1)
*)
(* 6d: 
i: 5
ii: let isWest x = ((x%4) = 3)
iii: let west = 3
*)
(* 6e:
i: impossible
ii: impossible
iii: impossible
 *)
