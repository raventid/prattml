type binop = Add | Sub | Mul | Div | Pow

type token = TNum of int | TOp of binop

let example_tokens = [ TNum 1
    ; TOp Add
    ; TNum 2
    ; TOp Sub
    ; TNum 3
    ; TOp Mul
    ; TNum 4
    ; TOp Add
    ; TNum 5
    ; TOp Div
    ; TNum 6
    ; TOp Pow
    ; TNum 7
    ; TOp Sub
    ; TNum 8
    ; TOp Mul
    ; TNum 9
    ]

let string_of_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Pow -> "Pow"

let string_of_token = function
  | TNum n -> "TNum " ^ string_of_int n
  | TOp op -> "TOp " ^ string_of_binop op

let print_token_stream token_stream = List.iter (fun token -> print_endline (string_of_token token)) token_stream 

let () = print_token_stream example_tokens
