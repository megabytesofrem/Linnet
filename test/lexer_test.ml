open Linnet
open Linnet.Lexer

module T = Token

let token_testable = 
  Alcotest.testable (fun pp t -> Fmt.pf pp "%s" (string_of_token t)) ( = )

let test_case_generator (input, expected) = 
  let name = Printf.sprintf "lexing: '%s'" input in
  Alcotest.test_case name `Quick (fun () ->
    let actual = tokenize input in
    Alcotest.(check (list token_testable)) "tokens match" expected actual
  )

let binary_cases = [
  ("1 + 2",       [T.Number 1; T.Plus; T.Number 2; T.EOF]);
  ("1 + 2 * 3",   [T.Number 1; T.Plus; T.Number 2; T.Star; T.Number 3; T.EOF]);
]

let unary_cases = [
  ("-5",          [T.Minus; T.Number 5; T.EOF]);
]

let suite = 
  let cases = binary_cases @ unary_cases in
  List.map test_case_generator cases

(* Test suite runner *)
let () =
  Alcotest.run "Lexer Tests" [
    ("Token Tests", suite);
  ] 