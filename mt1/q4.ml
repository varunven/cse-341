type funny_string_tree =
  | Empty
  | One of string
  | Node of funny_string_tree * funny_string_tree

let rec fold_left_tree f acc t =
  match t with
  | Empty -> acc
  | One s -> f acc s
  | Node (t1,t2) -> fold_left_tree f (fold_left_tree f acc t1) t2

let list_of_tree2 fs = fold_left_tree (fun acc x -> x::acc) [] fs

let tree = Node(Node(Node(One("a"), One("b")), One("c")), Node(One("d"), One("e")))

let has_string2 s fs = fold_left_tree (fun acc x -> if(x = s) then true else acc||false) false fs

let ex = has_string2 "s" tree 
let ex = has_string2 "a" tree 

let rec exists func fs = 
    match fs with 
        Empty -> false 
        | One s' -> func s'
        | Node(fs1, fs2) -> exists func fs1 || exists func fs2

let has_string3 s fs = exists (fun x -> if(x = s) then true else false) fs

let ex = has_string3 "s" tree 
let ex = has_string3 "a" tree 

let exists2 func fs = fold_left_tree func false fs

let ex = exists (fun x -> if(x = "s") then true else false) tree
let ex = exists (fun x -> if(x = "a") then true else false) tree