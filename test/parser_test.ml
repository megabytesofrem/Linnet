open Linnet
open Linnet.Lexer
open Linnet.Parser

(* Run the monadic parser computation *)
let run_parser p s =
  let tokens = tokenize s in
  match parse p tokens with
  | Ok result -> result
  | Error msg -> Alcotest.failf "Parse error on '%s': %s" s msg

let parse_expr_str s = run_parser (parse_expr ()) s
let parse_ty_str s = run_parser (parse_type ()) s
let parse_decl_str s = run_parser (parse_decl ()) s

(* --------------------------------------------------------- *)
(* Cozy Test Helpers                                         *)
(* --------------------------------------------------------- *)

let should_parse name input assertion =
  Alcotest.test_case name `Quick (fun () ->
      let expr = parse_expr_str input in
      assertion expr)

let should_parse_decl name input assertion =
  Alcotest.test_case name `Quick (fun () ->
      let decl = parse_decl_str input in
      assertion decl)

let should_parse_type name input assertion =
  Alcotest.test_case name `Quick (fun () ->
      let ty = parse_ty_str input in
      assertion ty)

let should_fail name input =
  Alcotest.test_case name `Quick (fun () ->
      let tokens = tokenize input in
      match parse (parse_expr ()) tokens with
      | Ok _ -> Alcotest.fail "Expected parse failure, but it succeeded!"
      | Error _ -> ())

(* --------------------------------------------------------- *)
(* Structural Assertions                                     *)
(* --------------------------------------------------------- *)

let assert_binop op expr =
  match expr with
  | Ast.Expr.Binary (o, _, _) when o = op -> ()
  | _ -> Alcotest.fail "Expected specific binary operation"

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

(* --------------------------------------------------------- *)
(* Test Suites                                               *)
(* --------------------------------------------------------- *)

let operator_tests =
  let open Ast.Expr in
  [
    should_parse "addition: 1 + 2" "1 + 2" (assert_binop Ast.BinOp.Add);
    should_parse "multiplication: 2 * 3" "2 * 3" (assert_binop Ast.BinOp.Mul);
    should_parse "precedence: 1 + 2 * 3" "1 + 2 * 3" assert_precedence;
    should_parse "application: f x y" "f x y" (fun expr ->
        match expr with
        | App (App (Ident "f", Ident "x"), Ident "y") -> ()
        | _ -> Alcotest.fail "Expected application (f x) y");
    should_parse "composition: f . g . h" "f . g . h" (fun expr ->
        match expr with
        | Ast.Expr.Binary
            ( Ast.BinOp.Compose,
              Ast.Expr.Ident "f",
              Ast.Expr.Binary
                (Ast.BinOp.Compose, Ast.Expr.Ident "g", Ast.Expr.Ident "h") ) ->
            ()
        | _ ->
            Alcotest.fail "Expected right-associative composition: f . (g . h)");
  ]

let type_tests =
  [
    should_parse_type "simple type: Int" "Int" (fun ty ->
        match ty with
        | Ast.Ty.Int -> ()
        | _ -> Alcotest.fail "Expected base type Int");
    should_parse_type "function type: Int -> String" "Int -> String" (fun ty ->
        match ty with
        | Ast.Ty.Fn (Ast.Ty.Int, Ast.Ty.String) -> ()
        | _ -> Alcotest.fail "Expected function type Int -> String");
    should_parse_type "Haskell-style parameterized type: Maybe Int" "Maybe Int"
      (fun ty ->
        match ty with
        | Ast.Ty.TyCons ("Maybe", [ Ast.Ty.Int ]) -> ()
        | _ -> Alcotest.fail "Expected parameterized type Maybe Int");
  ]

let decl_tests =
  [
    should_parse_decl "typeclass declaration"
      "class Eq[a] { fn eq[a] : a -> a -> Bool }" (fun decl ->
        match decl with
        | Ast.Decl.Class
            {
              class_name = "Eq";
              type_params = [ "a" ];
              methods = [ ("eq", [ "a" ], ty) ];
            } -> (
            match ty with
            | Ast.Ty.Fn (Ast.Ty.Var "a", Ast.Ty.Fn (Ast.Ty.Var "a", Ast.Ty.Bool))
              ->
                ()
            | _ -> Alcotest.fail "Expected method type a -> a -> Bool")
        | _ -> Alcotest.fail "Expected typeclass declaration");
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

(* --------------------------------------------------------- *)
(* Test Suite Runner                                         *)
(* --------------------------------------------------------- *)
let () =
  Alcotest.run "Parser Tests"
    [
      ("Operators", operator_tests);
      ("Types", type_tests);
      ("Declarations", decl_tests);
      ("Lambdas", lambda_tests);
      ("Collections", collection_tests);
      ("Errors", error_tests);
    ]
