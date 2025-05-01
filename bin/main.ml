type binop = Add | Sub | Mul | Div | Pow

type token = TNum of int | TOp of binop | TLeftParen | TRightParen

type expr = Num of int | Op of expr * binop * expr

(* higher precedence binds stronger *)
let precedence = function
| Add | Sub -> 1
| Mul | Div -> 2
| Pow -> 3

let is_right_associative = function
| Add | Sub | Mul | Div -> false
| Pow -> true

let rec prefix (tokens : token list) : (expr * token list) option = match tokens with
  [] -> None
| TNum n :: rest -> Some (Num n, rest)
| TOp Sub :: TNum n :: rest -> Some (Num (-n), rest) (* Unary negation, but grammar is not OK, what if `----4`? *)
| TLeftParen :: rest -> grouping_expression rest
| TRightParen :: _rest -> None (* Does not make any sense on it's own, so parsed as a grouping operator *)
|  _ -> None  (* Something malformed in input *)

and grouping_expression (tokens : token list) : (expr * token list) option =   match pratt 0 tokens with
  | None -> None
      | Some (expr, TRightParen :: tokens_after_right_paren) -> Some (expr, tokens_after_right_paren)
      | Some (_expr, _tokens_missing_right_paren) -> None (* We are missing a right paren, so we are just stopping the parser *)

and pratt (precedence_limit : int) (tokens : token list) : (expr * token list) option = match prefix tokens with
  None -> None
| Some (left_expr, tokens_after_prefix) -> pratt_loop precedence_limit left_expr tokens_after_prefix

and pratt_loop (precendence_limit : int) (left_expression : expr) (tokens : token list) : (expr * token list) option =
  (* Next token is an operator, let's figure out its precedence *)
  match tokens with
 | TOp operator :: tokens_after_operator -> 
    let op_precedence = precedence operator in
    let finalized_precedence = 
      if is_right_associative operator then (* do not increase precedence limit if operator is right assocc*)
        op_precedence - 1
      else
        op_precedence in
    (* -- If the precedence of the operator is greater than the limit, *)
    (* -- we can parse it. *)
    (* -- Otherwise, we should stop parsing. *)
    (* -- (Note: this is a bit different from the Pratt paper.) *)
    (* -- We can also check for associativity here. *)
    (* -- If it's right associative, we need to decrement the precedence. *)
    if op_precedence > precendence_limit then
      (* we can parse it, spawn a child pratt parser *)
      match pratt finalized_precedence tokens_after_operator with
      | Some (right_expr, tokens_after_child) ->
        let new_expression = Op (left_expression, operator, right_expr) in
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
      Some (left_expression, tokens)
   | _ -> 
      (* Either we ran out of tokens or found something *)
      (* that's not an operator. Let's return what we have. *)
      Some (left_expression, tokens)

(*  *)
let binaryOp (left : expr) (op : binop) (precedence : int) (tokens_after_op : token list) : (expr * token list) option  =  match pratt precedence tokens_after_op with
    | None -> None
    | Some (right, tokens_after_right) -> Some (Op (left, op, right), tokens_after_right)

(*
    Example: 5++
    
    Token expectations:
    
        expr Increment
        ^^^^^^^^^ already parsed
*)
(* let postfixIncrement (left : expr) (op: binop) (tokens_after_op : token list) : (expr * token list) option =  *)
        (* -- We're guaranteed to have parsed the `++` already *)
        (* -- Nothing to do here! *)
        (* Some (UnaryOp Increment left, tokens_after_op) *)

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

let example_tokens2 = [ TNum 1
    ; TOp Add
    ; TNum 2
    ; TOp Pow
    ; TNum 3
    ; TOp Pow
    ; TNum 4
    ; TOp Add
    ; TNum 5
    ; TOp Div
    ; TNum 6
]

let example_tokens3 = [ TNum 1
    ; TOp Add
    ; TLeftParen
    ; TOp Sub
    ; TNum 2
    ; TOp Sub
    ; TNum 3
    ; TRightParen
    ; TOp Mul
    ; TNum 4
    ; TOp Add
    ; TNum 5
    ; TOp Div
    ; TNum 6
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
  | TLeftParen -> "TLeftParen"
  | TRightParen -> "TRightParen"

let print_token_stream token_stream = List.iter (fun token -> print_endline (string_of_token token)) token_stream 

let () = 
  print_endline "Token stream:";
  print_token_stream example_tokens;

  print_endline "\nParsed expression:";
  print_expr (parse example_tokens);

  print_endline "\nExpression Tree:";
  print_expr_tree (parse example_tokens);

  print_endline "\nSecond example token stream:";
  print_token_stream example_tokens2;

  print_endline "\nParsed expression:";
  print_expr (parse example_tokens2);

  print_endline "\nThird example token stream:";
  print_token_stream example_tokens3;

  print_endline "\nParsed expression:";
  print_expr (parse example_tokens3);