(* 1 *)
type maze = Finish | DeadEnd | Forward of maze * maze

let sample_maze_true = Forward(DeadEnd,Forward(Finish,Forward(DeadEnd,DeadEnd)))
let sample_maze_false = Forward(DeadEnd,Forward(DeadEnd,Forward(DeadEnd,DeadEnd)))

let rec has_a_solution m =
    match m with 
        Finish -> true
        | DeadEnd -> false 
        | Forward(m1, m2) -> has_a_solution m1 || has_a_solution m2 

let sample_has_a_solution_true = has_a_solution sample_maze_true
let sample_has_a_solution_false = has_a_solution sample_maze_false

type dir = Left | Right 

let solve_maze maze = 
    let rec solve_maze_helper(m, acc) =
        match m with 
            Finish -> Some acc
            | DeadEnd -> None
            | Forward(m1, m2) -> 
                let left = solve_maze_helper(m1, [Left]@acc) in 
                let right = solve_maze_helper(m2, [Right]@acc) in 
                if(left <> None) then left
                else if(right <> None) then right
                else None
    in 
    solve_maze_helper(maze, [])

let sample_maze_left_left = Forward(Forward(Finish, DeadEnd), DeadEnd)
let sample_maze_left_left_solve = solve_maze sample_maze_left_left

type maze2 =
| End of bool (* true means finish; false means dead-end *)
| Branch of maze2 list (* any number of next paths *)

let rec maze_to_maze2(m) =
    match m with 
        Finish -> End(true)
        | DeadEnd -> End(false)
        | Forward(m1, m2) -> Branch([maze_to_maze2(m1);maze_to_maze2(m2)]) 

let maze2_left_left = maze_to_maze2(sample_maze_left_left)

(* 2 *)