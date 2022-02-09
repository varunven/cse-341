type funny_string_tree =
  | Empty
  | One of string
  | Node of funny_string_tree * funny_string_tree 

let rec has_string (s:string) (fs:funny_string_tree) = 
    match fs with 
        Empty -> false 
        | One s' -> if(s=s') then true else false 
        | Node(fs1, fs2) -> has_string s fs1 || has_string s fs2 

let rec list_of_tree fs = 
    match fs with 
        Empty -> [] 
        | One s -> [s]
        | Node(fs1, fs2) -> (list_of_tree fs1)@(list_of_tree fs2)

let tree = Node(Node(Node(One("a"), One("b")), One("c")), Node(One("d"), One("e")))
let t = has_string "a" tree
let f = has_string "x" tree
let l = list_of_tree tree