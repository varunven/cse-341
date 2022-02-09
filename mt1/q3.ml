type funny_string_tree =
  | Empty
  | One of string
  | Node of funny_string_tree * funny_string_tree

let example = Node (Node (Node (Node (Empty, Empty), 
                                Node (Empty, Empty)),
                          Empty), 
                    Node (Empty, One "hello"))

let example_answer = Node (Empty, 
                           Node (Empty, One "hello"))

let rec has_no_double_empty fs = 
    match fs with 
        Empty -> false
        | One s -> true 
        | Node(fs1, fs2) -> 
            match fs1 with 
                Empty -> 
                    (match fs2 with 
                        Empty -> false
                        | _ -> has_no_double_empty fs2)
                | One s' -> has_no_double_empty fs2
                | Node(fs1a, fs1b) -> 
                has_no_double_empty fs1a && has_no_double_empty fs1b && has_no_double_empty fs2

let ex = has_no_double_empty example

let ts = [Node(One("a"), One("b"));Node(One("c"), One("d"));Node(One("e"), One("f"));Node(One("g"), One("h"))]

let ts_ex = List.map has_no_double_empty ts

let rec make_no_all_empty_nodes fs = 
    match fs with 
        Empty -> Empty
        | One s -> One s 
        | Node(fs1, fs2) -> 
            let one = make_no_all_empty_nodes fs1 in 
            let two = make_no_all_empty_nodes fs2 in 
            match one with 
                Empty -> 
                    (match two with 
                        Empty -> Empty
                        | _ -> Node(one, two))
                | One s' -> Node(one, two)
                | Node(fs1a, fs1b) -> Node(one, two)

let tree = Node(Node(Node(One("a"), One("b")), One("c")), Node(One("d"), One("e")))

let ex_emp = make_no_all_empty_nodes example
let ex_emp = (make_no_all_empty_nodes tree) = tree