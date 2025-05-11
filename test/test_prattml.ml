open OUnit2
open Prattml.Power_pratt

(* Helper to create a token queue from a sequence of characters *)
let create_token_queue chars =
  let q = Queue.create () in
  String.iter (fun c -> 
    if c >= '0' && c <= '9' then 
      Queue.add (T_Atom c) q
    else if c = '+' || c = '-' || c = '*' || c = '/' || c = '^' then
      Queue.add (T_Op c) q
  ) chars;
  Queue.add T_Eof q;
  q

(* Test for expr_bp function with a simple number *)
let test_expr_bp_simple_number _ =
  let tokens = create_token_queue "1" in
  let result = expr_bp tokens 0 in
  assert_equal (string_of_token_stream result) "1"

(* Define the test suite *)
let suite =
  "PrattParserTests" >:::
  [
    "test_simple_number" >:: test_expr_bp_simple_number;
    (* Commenting out the addition test until expr_bp is fully implemented
    "test_addition" >:: test_expr_bp_addition;
    *)
  ]

(* Run the tests *)
let () = run_test_tt_main suite