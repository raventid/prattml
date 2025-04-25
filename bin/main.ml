type binop = Add | Sub | Mul | Div | Pow

type token = TNum of int | TOp of binop

let precedence = function
| Add | Sub -> 1
| Mul | Div -> 2
| Pow -> 3

type expr = Num of int | Op of expr * binop * expr

let prefix (tokens : token list) : (expr * token list) option = match tokens with
  [] -> None
| TNum n :: rest -> Some (Num n, rest)
|  _ -> None

let rec pratt (precedence_limit : int) (tokens : token list) : (expr * token list) option = match prefix tokens with
  None -> None
| Some (left_expr, tokens_after_prefix) -> pratt_loop precedence_limit left_expr tokens_after_prefix

and pratt_loop (precendence_limit : int) (expression : expr) (tokens : token list) : (expr * token list) option =
  (* Next token is an operator, let's figure out its precedence *)
  match tokens with
 | TOp operator :: tokens_after_operator -> 
    let op_precedence = precedence operator in
    if op_precedence > precendence_limit then
      (* we can parse it, spawn a child pratt parser *)
      match ( pratt op_precedence tokens_after_operator ) with
      | Some (right_expr, tokens_after_child) ->
        let new_expression = Op (expression, operator, right_expr) in
          (* -- There might be more on our level *)
          (* -- (like in 1+2-3), so let's loop. *)
          pratt_loop precendence_limit new_expression tokens_after_child
      | None -> 
        (* -- No more tokens, we are done. *)
        None
    else
      (* We shouldn't parse this op. *)
      (* Return what we have. *)
      (* (Note our token list points at the op, not at the token after.) *)
      Some (expression, tokens)
   | _ -> 
      (* Either we ran out of tokens or found something *)
      (* that's not an operator. Let's return what we have. *)
      Some (expression, tokens)

let parse tokens = match (pratt 0 tokens) with
  Some (expr, _tokens_after_expression) -> Some expr
| None -> None 

let rec string_of_expr = function
  | Num n -> string_of_int n
  | Op (left, op, right) -> 
      "(" ^ string_of_expr left ^ " " ^ 
      (match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Pow -> "^") ^ 
      " " ^ string_of_expr right ^ ")"

let print_expr (parsed_expression: expr option) : unit =
  match parsed_expression with
  | None -> print_endline "No valid expression parsed."
  | Some expr -> print_endline (string_of_expr expr)

let rec print_expr_tree_helper expr prefix is_last =
  match expr with
  | Num n ->
      print_string prefix;
      print_string (if is_last then "└── " else "├── ");
      print_endline (string_of_int n)
  | Op (left, op, right) ->
      print_string prefix;
      print_string (if is_last then "└── " else "├── ");
      print_string (match op with
                    | Add -> "+"
                    | Sub -> "-"
                    | Mul -> "*"
                    | Div -> "/"
                    | Pow -> "^");
      print_endline "";
      
      let new_prefix = prefix ^ (if is_last then "    " else "│   ") in
      print_expr_tree_helper left new_prefix false;
      print_expr_tree_helper right new_prefix true

let print_expr_tree (parsed_expression: expr option) : unit =
  match parsed_expression with
  | None -> print_endline "No valid expression parsed."
  | Some expr -> 
      print_endline "Expression Tree:";
      print_expr_tree_helper expr "" true

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

let () = 
  print_endline "Token stream:";
  print_token_stream example_tokens;

  print_endline "\nParsed expression:";
  print_expr (parse example_tokens);

  print_endline "\nExpression Tree:";
  print_expr_tree (parse example_tokens)