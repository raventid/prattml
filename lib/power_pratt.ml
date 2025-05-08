(* This Pratt implementation is similar to the presented in Main,
the difference is that it uses a power concept and precedence table uses
2 values right and left power, instead of just 1 *)

type binop = Add | Sub | Mul | Div | Pow

type token = Atom of char | Op of char | Eof

let precedence = function
  | Add | Sub -> (1, 2)
  | Mul | Div -> (3, 4)
  | Pow -> (5, 6)
  


(* s-expression form to emulate lexing/scanning result *)
type token_stream = Atom of char | Cons of char * token_stream list

(* Format token_stream to string representation *)
let rec string_of_token_stream = function
  | Atom c -> String.make 1 c
  | Cons (head, rest) ->
      let rest_str = String.concat " " (List.map string_of_token_stream rest) in
      "(" ^ (String.make 1 head) ^ (if rest_str = "" then "" else " " ^ rest_str) ^ ")"


let ts = Cons ('a', [Atom 'b'; Cons ('c', [Atom 'd'; Atom 'e'])])
let parse = string_of_token_stream ts

(* Example: Represent (+ 1 (* 2 3)) as a token_stream *)
let example_expression = 
  Cons ('+', [
    Atom '1'; 
    Cons ('*', [
      Atom '2'; 
      Atom '3'
    ])
  ])

(* Print the expression *)
let () = print_endline (string_of_token_stream example_expression)
