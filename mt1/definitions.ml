type funny_string_tree =
  | Empty
  | One of string
  | Node of funny_string_tree * funny_string_tree 

