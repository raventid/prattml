(* This Pratt implementation is similar to the presented in Main,
the difference is that it uses a power concept and precedence table uses
2 values right and left power, instead of just 1 *)

type binop = Add | Sub | Mul | Div | Pow

type token = T_Atom of char | T_Op of char | T_Eof

let precedence = function
  | Add | Sub -> (1, 2)
  | Mul | Div -> (3, 4)
  | Pow -> (5, 6)
  


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

let expr_bp (token_stream : token Queue.t) : s_expression =  
  let lhs = match Queue.pop token_stream with
    | T_Atom(it) -> Atom(it)
    | _ -> Atom('#') in (* bad token *)
      try
        while true do
          let _ = match Queue.pop token_stream with
            | T_Eof -> raise Break (* break if end of stream *)
            | T_Op(it) -> it
            | _ -> '#' (* bad token *) in

            () (* TODO *)
        done
      with
      | Break -> (); (* return bad token for now, when we see eof *)

      lhs



let example_s_expression = 
  Cons ('+', [
    Atom '1'; 
    Cons ('*', [
      Atom '2'; 
      Atom '3'
    ])
  ])
