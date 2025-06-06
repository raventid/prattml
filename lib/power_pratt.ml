(* This implementation is based on the Pratt parsing variation technique *)
(* described in the article by Matklad: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html *)

(* This Pratt implementation is similar to the presented in Main,
the difference is that it uses a power concept and precedence table uses
2 values right and left power, instead of just 1 *)

(* Also this implementation shows some of the imperative features of Ocaml PL *)

type token = T_Atom of char | T_Op of char | T_Eof

let t2s = function
  | T_Atom c -> String.make 1 c
  | T_Op c -> String.make 1 c
  | T_Eof -> "EOF"

let prefix_binding_power (op : char) : (unit * int) = match op with 
        | '+' | '-' -> ((), 9)
        | _ -> exit 1 (* invalid operator, just panic and crash the program *)
    
let postfix_binding_power (op : char) : (int * unit) option = match op with
        | '!' | '[' -> Some (11, ())
        | _ -> None (* invalid operator, just panic and crash the program *)

(* Infix binding power is a tuple of left and right binding power *)
(* The left binding power is used to determine when to stop parsing further expressions *)
(* The right binding power is used to determine the precedence of the current operator *)

let infix_binding_power (op : char) : (int * int) option = match op with
        | '=' -> Some (2, 1)
        | '?' -> Some (4, 3)
        | '+' | '-' -> Some (5, 6)
        | '*' | '/' -> Some (7, 8)
        | '.' -> Some (14, 13)
        | _ -> None

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
    | T_Op('(') -> let lhs = expr_bp token_stream 0 in
                  (match Queue.pop token_stream with
                    | T_Op(')') -> lhs
                    | _ -> raise BadToken) (* bad token *)
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
                if op = '[' then
                  let rhs = expr_bp token_stream 0 in
                    Queue.pop token_stream |> (fun op -> if op <> T_Op(']') then raise (Failure ("Expected ']' but got " ^ (t2s op)))); (* consume the closing bracket *)
                    lhs_ref := Cons(op, [!lhs_ref; rhs])
                else
                  lhs_ref := Cons(op, [!lhs_ref]);
            | None -> (* no postfix operator, let's continue with infix operators *)
                match infix_binding_power op with
                | Some (left_bp, right_bp) ->
                    if left_bp < minimal_binding_power then
                      raise LeftBindingPowerIsTooWeak (* break if precedence is lower than limit *)
                    else
                      Queue.pop token_stream |> ignore; (* consume the operator token *)

                      (* ternary operator *)
                      if op == '?' then
                        let mhs = expr_bp token_stream 0 in
                          Queue.pop token_stream |> ignore; (* consume the ':' token *)
                          let rhs = expr_bp token_stream right_bp in
                            lhs_ref := Cons(op, [!lhs_ref; mhs; rhs])
                      else
                        (* check if we need to increase precedence limit *)
                        let rhs = expr_bp token_stream right_bp in
                          lhs_ref := Cons(op, [!lhs_ref; rhs]);
                | None -> raise EndOfTokenStream
        done;
        !lhs_ref (* Return the final value *)
      with
      | LeftBindingPowerIsTooWeak -> !lhs_ref (* return the current value when we break *)
      | EndOfTokenStream -> !lhs_ref (* return the current value when we reach the end of the stream *)
      | BadToken -> raise BadToken (* raise an error for bad token, I create token stream manually, so I assume valid programs all the time *)

