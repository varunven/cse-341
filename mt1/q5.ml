type funny_string_tree =
  | Empty
  | One of string
  | Node of funny_string_tree * funny_string_tree 

let rec tree_of_list ss =
  match ss with
  | [] -> Empty
  | s::ss' -> Node (One s, tree_of_list ss')

let tree_of_list2 ss =
  let rec loop (acc,ss) =
    match ss with
    | [] -> acc
    | s::ss' -> loop (Node (One s, acc),ss') 
  in
  loop (Empty,ss)

let example =  ["a";"b";"c";"d";"e"]

let x = tree_of_list example
let y = tree_of_list2 example

(* 5.2:
They are not approximately equivalent because one is a head-recursive function while the other is tail-recursive, so the ordering of the nodes is reversed.
For example, if the list passed in was ["a";"b"], then tree_of_list would return Node (One "a", Node (One "b", Empty)), but tree_of_list2 would return Node (One "b", Node (One "a", Empty))
 *)

(* 5.3: 

let rec list_of_tree fs = 
    match fs with 
        Empty -> [] 
        | One s -> [s]
        | Node(fs1, fs2) -> (list_of_tree fs1)@(list_of_tree fs2)

let x = list_of_tree(tree_of_list example)
let y = list_of_tree(tree_of_list2 example)

let tree = Node(Node(Node(Node(One("a"), One("b")), One("c")), Node(One("d"), One("e"))), Empty)

let z = tree_of_list(list_of_tree tree)
let a = tree_of_list2(list_of_tree tree)
 *)