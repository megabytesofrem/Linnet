open Linnet
open Linnet.Lexer
open Linnet.Parser

(* kleisli composition *)
let ( >> ) f g x = g (f x)

let parse s : Ast.Expr.t parser_result =
  tokenize s |> create_parse_state |> parse_expr

let unwrap = function
  | Ok expr -> expr
  | Error msg -> Alcotest.failf "Parse error: %s" msg

(* Test helpers with descriptive names *)
let should_parse name input assertion =
  Alcotest.test_case name `Quick (fun () ->
      let expr = unwrap (parse input) in
      assertion expr)

let should_fail name input =
  Alcotest.test_case name `Quick (fun () ->
      match parse input with
      | Ok _ -> Alcotest.fail "Expected parse failure"
      | Error _ -> ())

(* Assertion helpers *)
let assert_binop op expr =
  match expr with
  | Ast.Expr.Binary (o, _, _) when o = op -> ()
  | _ ->
      Alcotest.failf "Expected binary operation %s"
        (match op with
        | Ast.BinOp.Add -> "Add"
        | Ast.BinOp.Mul -> "Mul"
        | _ -> "")

let assert_lambda expr =
  match expr with
  | Ast.Expr.Lam _ -> ()
  | _ -> Alcotest.fail "Expected lambda expression"

let assert_list expr =
  match expr with
  | Ast.Expr.List _ -> ()
  | _ -> Alcotest.fail "Expected list expression"

let assert_precedence expr =
  match expr with
  | Ast.Expr.Binary (Ast.BinOp.Add, _, Ast.Expr.Binary (Ast.BinOp.Mul, _, _)) ->
      ()
  | _ -> Alcotest.fail "Wrong precedence: expected (1 + (2 * 3))"

(* Tests *)
let operator_tests =
  let open Ast.Expr in
  [
    should_parse "addition: 1 + 2" "1 + 2" (assert_binop Add);
    should_parse "multiplication: 2 * 3" "2 * 3" (assert_binop Mul);
    should_parse "precedence: 1 + 2 * 3" "1 + 2 * 3" assert_precedence;
    should_parse "application: f x y" "f x y" (fun expr ->
        match expr with
        | App (App (Ident "f", Ident "x"), Ident "y") -> ()
        | _ -> Alcotest.fail "Expected application f x y");
    should_parse "composition: f . g . h" "f . g . h" (fun expr ->
        match expr with
        | Binary (Compose, Binary (Compose, Ident "f", Ident "g"), Ident "h") ->
            ()
        | _ -> Alcotest.fail "Expected composition f . g . h");
  ]

let lambda_tests =
  let open Ast.Expr in
  [
    should_parse "simple lambda: \\x -> x" "\\x -> x" assert_lambda;
    should_parse "multi-param: \\x y -> x" "\\x y -> x" (fun expr ->
        match expr with
        | Lam ([ "x"; "y" ], _) -> ()
        | _ -> Alcotest.fail "Expected lambda with params [x; y]");
  ]

let collection_tests =
  let open Ast.Expr in
  [
    should_parse "empty list: []" "[]" (fun expr ->
        match expr with
        | List [] -> ()
        | _ -> Alcotest.fail "Expected empty list");
    should_parse "tuple: (1, 2, 3)" "(1, 2, 3)" (fun expr ->
        match expr with
        | Tuple xs -> ()
        | _ -> Alcotest.fail "Expected tuple (1, 2, 3)");
    should_parse "list: [1, 2, 3]" "[1, 2, 3]" assert_list;
  ]

let error_tests =
  [
    should_fail "unclosed parenthesis" "(1 + 2";
    should_fail "unexpected token" "+ 1";
  ]

(* Test suite runner *)
let () =
  Alcotest.run "Parser Tests"
    [
      ("Operators", operator_tests);
      ("Lambdas", lambda_tests);
      ("Collections", collection_tests);
      ("Errors", error_tests);
    ]
