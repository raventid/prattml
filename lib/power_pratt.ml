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


let ts = Cons ('a', [Atom 'b'; Cons ('c', [Atom 'd'; Atom 'e'])])
let parse = string_of_token_stream ts


let make_token_stream_example1 : token Queue.t = 
 let q = Queue.create () in
  Queue.push (T_Atom '1') q;
  Queue.push (T_Atom '+') q;
  Queue.push (T_Atom '2') q;
  Queue.push (T_Atom '*') q;
  Queue.push (T_Atom '3') q;
  Queue.push T_Eof q;
  q


(* helper types *)
exception Break

let rec expr_bp (token_stream : token Queue.t) (precedence_limit : int) : s_expression =  
  let lhs_ref = ref (match Queue.pop token_stream with
    | T_Atom(it) -> Atom(it)
    | _ -> Atom('#')) in (* bad token *)
      try
        while true do
          let op = match Queue.pop token_stream with
            | T_Eof -> raise Break (* break if end of stream *)
            | T_Op(it) -> it
            | _ -> '#' (* bad token *) in

            let (left_bp, right_bp) = infix_binding_power op in
            if left_bp < precedence_limit then
              raise Break (* break if precedence is lower than limit *)
            else
              let rhs = expr_bp token_stream right_bp in
              lhs_ref := Cons(op, [!lhs_ref; rhs])
              (* check if we need to break the loop *)
        done;
        !lhs_ref (* Return the final value *)
      with
      | Break -> !lhs_ref (* return the current value when we break *)


let example_s_expression = 
  Cons ('+', [
    Atom '1'; 
    Cons ('*', [
      Atom '2'; 
      Atom '3'
    ])
  ])
