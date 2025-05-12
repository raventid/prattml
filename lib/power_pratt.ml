(* This Pratt implementation is similar to the presented in Main,
the difference is that it uses a power concept and precedence table uses
2 values right and left power, instead of just 1 *)

type token = T_Atom of char | T_Op of char | T_Eof

let prefix_binding_power (op : char) : (unit * int) = match op with 
        | '+' | '-' -> ((), 5)
        | _ -> exit 1 (* invalid operator, just panic and crash the program *)
    
let postfix_binding_power (op : char) : (int * unit) option = match op with
        | '!' -> Some (7, ())
        | _ -> None (* invalid operator, just panic and crash the program *)

(* Infix binding power is a tuple of left and right binding power *)
(* The left binding power is used to determine when to stop parsing further expressions *)
(* The right binding power is used to determine the precedence of the current operator *)

let infix_binding_power (op : char) : (int * int) = match op with
        | '+' | '-' -> (1, 2)
        | '*' | '/' -> (3, 4)
        | '.' -> (10, 9)
        | _ -> exit 1 (* invalid operator, just panic and crash the program *)

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
exception LeftBindingPowerIsTooWeak
exception BadToken

(* The main function to parse an expression using Pratt's algorithm *)
(* It uses a token stream and a precedence limit to build the expression tree *)
(* The function returns an s_expression representing the parsed expression *)
(* The precedence limit is used to determine when to stop parsing further expressions *)

let rec expr_bp (token_stream : token Queue.t) (minimal_binding_power : int) : s_expression =  
  let lhs_ref = ref (match Queue.pop token_stream with
    | T_Atom(it) -> Atom(it)
    | T_Op(it) -> let ((), right_bp) = prefix_binding_power it in
                  let lhs = expr_bp token_stream right_bp in
                  Cons(it, [lhs])
    | _ -> raise BadToken) in (* bad token *)
      try
        while true do
          let op = match Queue.peek token_stream with
            | T_Eof -> raise EndOfTokenStream (* break if end of stream *)
            | T_Op(it) -> it
            | _ -> raise BadToken (* bad token *) in

            match postfix_binding_power op with
            | Some (left_bp, ()) ->
               if left_bp < minimal_binding_power then
                raise LeftBindingPowerIsTooWeak (* break if precedence is lower than limit *)
               else
                Queue.pop token_stream |> ignore; (* consume the postfix operator token *)
                lhs_ref := Cons(op, [!lhs_ref]);
            | None -> (* no postfix operator, let's continue with infix operators *)
                let (left_bp, right_bp) = infix_binding_power op in
                  if left_bp < minimal_binding_power then
                    raise LeftBindingPowerIsTooWeak (* break if precedence is lower than limit *)
                  else
                    Queue.pop token_stream |> ignore; (* consume the operator token *)
                    (* check if we need to increase precedence limit *)
                    let rhs = expr_bp token_stream right_bp in
                      lhs_ref := Cons(op, [!lhs_ref; rhs]);
        done;
        !lhs_ref (* Return the final value *)
      with
      | LeftBindingPowerIsTooWeak -> !lhs_ref (* return the current value when we break *)
      | EndOfTokenStream -> !lhs_ref (* return the current value when we reach the end of the stream *)
      | BadToken -> raise BadToken (* raise an error for bad token, I create token stream manually, so I assume valid programs all the time *)

