(* This Pratt implementation is similar to the presented in Main,
the difference is that it uses a power concept and precedence table uses
2 values right and left power, instead of just 1 *)

type token = T_Atom of char | T_Op of char | T_Eof

let infix_binding_power (op : char) : (int * int) = match op with
        | '+' | '-' -> (1, 2)
        | '*' | '/' -> (3, 4)
        | _ -> (-1,-1) (* invalid operator *)

(* s-expression form to emulate lexing/scanning result *)
type s_expression = Atom of char | Cons of char * s_expression list

(* Format token_stream to string representation *)
let rec string_of_token_stream = function
  | Atom c -> String.make 1 c
  | Cons (head, rest) ->
      let rest_str = String.concat " " (List.map string_of_token_stream rest) in
      "(" ^ (String.make 1 head) ^ (if rest_str = "" then "" else " " ^ rest_str) ^ ")"

let example_s_expression = 
  Cons ('+', [
    Atom '1'; 
    Cons ('*', [
      Atom '2'; 
      Atom '3'
    ])
  ])
let parse = string_of_token_stream example_s_expression


(* helper types *)
exception EndOfTokenStream
exception ReturnCurrentExpression
exception BadToken

(* The main function to parse an expression using Pratt's algorithm *)
(* It uses a token stream and a precedence limit to build the expression tree *)
(* The function returns an s_expression representing the parsed expression *)
(* The precedence limit is used to determine when to stop parsing further expressions *)

let rec expr_bp (token_stream : token Queue.t) (precedence_limit : int) : s_expression =  
  let lhs_ref = ref (match Queue.pop token_stream with
    | T_Atom(it) -> Atom(it)
    | _ -> raise BadToken) in (* bad token *)
      try
        while true do
          let op = match Queue.peek token_stream with
            | T_Eof -> raise EndOfTokenStream (* break if end of stream *)
            | T_Op(it) -> it
            | _ -> raise BadToken (* bad token *) in

            let (left_bp, right_bp) = infix_binding_power op in
              if left_bp < precedence_limit then
                raise ReturnCurrentExpression (* break if precedence is lower than limit *)
              else
                Queue.pop token_stream |> ignore; (* consume the operator token *)
                (* check if we need to increase precedence limit *)
                let rhs = expr_bp token_stream right_bp in
                  lhs_ref := Cons(op, [!lhs_ref; rhs])
        done;
        !lhs_ref (* Return the final value *)
      with
      | ReturnCurrentExpression -> !lhs_ref (* return the current value when we break *)
      | EndOfTokenStream -> !lhs_ref (* return the current value when we reach the end of the stream *)
      | BadToken -> raise BadToken (* raise an error for bad token, I create token stream manually, so I assume valid programs all the time *)

