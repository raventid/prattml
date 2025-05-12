open OUnit2
open Prattml.Power_pratt

(* Helper to create a token queue from a sequence of characters *)
let create_token_queue chars =
  let q = Queue.create () in
  String.iter (fun c -> 
    if c >= '0' && c <= '9' || (c >= 'a' && c <= 'z') then 
      Queue.add (T_Atom c) q
    else if c = '+' || c = '-' || c = '*' || c = '/' || c = '^' || c = '.' then
      Queue.add (T_Op c) q
    else if c != ' ' then (* ignore spaces but handle other chars *)
      failwith ("Unexpected character: " ^ String.make 1 c)
  ) chars;
  Queue.add T_Eof q;
  q

(* Helper function to parse an expression string *)
let expr s = 
  let tokens = create_token_queue s in
  expr_bp tokens 0

(* Test for expr_bp function with a simple number *)
let test_expr_bp_simple_number _ =
let result = expr "1" in
  assert_equal (string_of_token_stream result) "1"

(* Test for arithmetic expression with operator precedence *)
let test_expr_bp_precedence _ =
  let result = expr "1 + 2 * 3" in
  assert_equal (string_of_token_stream result) "(+ 1 (* 2 3))"

(* Test for multi-operator expression with complex precedence *)
let test_expr_bp_complex _ =
  let result = expr "a + b * c * d + e" in
  assert_equal (string_of_token_stream result) "(+ (+ a (* (* b c) d)) e)"

(* Test for function composition operator *)
let test_function_composition _ =
  let result = expr "f . g . h" in
  assert_equal (string_of_token_stream result) "(. f (. g h))"

(* Test for complex expression with function composition and arithmetic operators *)
let test_mixed_operators _ =
  let result = expr "1 + 2 + f . g . h * 3 * 4" in
  assert_equal (string_of_token_stream result) "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))"

(* Define the test suite *)
let suite =
  "PrattParserTests" >:::
  [
    "test_simple_number" >:: test_expr_bp_simple_number;
    "test_precedence" >:: test_expr_bp_precedence;
    "test_complex" >:: test_expr_bp_complex;
    "test_function_composition" >:: test_function_composition;
    "test_mixed_operators" >:: test_mixed_operators;
  ]

(* Run the tests *)
let () = run_test_tt_main suite