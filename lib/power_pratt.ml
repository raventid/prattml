type binop = Add | Sub | Mul | Div | Pow

type token = Atom of char | Op of char | Eof

let precedence = function
  | Add | Sub -> (1, 2)
  | Mul | Div -> (3, 4)
  | Pow -> (5, 6)

let parse = "Checking parser"