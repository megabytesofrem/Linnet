open Linnet
open Linnet.Lexer
module T = Token

let token_testable =
  Alcotest.testable (fun pp t -> Fmt.pf pp "%s" (string_of_token t)) ( = )

(* Automate test suite cases *)
let test_case_generator (input, expected) =
  let name = Printf.sprintf "lexing: '%s'" input in
  Alcotest.test_case name `Quick (fun () ->
      let actual = tokenize input in
      Alcotest.(check (list token_testable)) "tokens match" expected actual)

(* Test cases *)

let binary_cases =
  [
    ("1 + 2", [ T.Number 1; T.Plus; T.Number 2; T.EOF ]);
    ("1 + 2 * 3", [ T.Number 1; T.Plus; T.Number 2; T.Star; T.Number 3; T.EOF ]);
  ]

let unary_cases = [ ("-5", [ T.Minus; T.Number 5; T.EOF ]) ]

let syntax_cases = 
  [
    ("let x = 10",      [T.Let; T.Ident "x"; T.Eq; T.Number 10; T.EOF]);
    ("let x: Int = 10", [T.Let; T.Ident "x"; T.Colon; T.Ident "Int"; T.Eq; T.Number 10; T.EOF]);
    ("if x > 0 then x else -x", 
      [T.If; T.Ident "x"; T.Greater; T.Number 0; T.Then; T.Ident "x"; T.Else; T.Minus; T.Ident "x"; T.EOF]);
    ("\\x y -> x + y", [T.Backslash; T.Ident "x"; T.Ident "y"; T.Arrow; T.Ident "x"; T.Plus; T.Ident "y"; T.EOF]);

    (* lists, tuples, unit *)
    ("[1, 2, 3]",   [T.LBrack; T.Number 1; T.Comma; T.Number 2; T.Comma; T.Number 3; T.RBrack; T.EOF]);
    ("(\"a\", \"b\")", [T.LParen; T.String "a"; T.Comma; T.String "b"; T.RParen; T.EOF]);
    ("()",          [T.LParen; T.RParen; T.EOF]);

    (* function application *)
    ("f x y",       [T.Ident "f"; T.Ident "x"; T.Ident "y"; T.EOF]);
    ("f (x y) z",   [T.Ident "f"; T.LParen; T.Ident "x"; T.Ident "y"; T.RParen; T.Ident "z"; T.EOF]);
    ("f $ x y",     [T.Ident "f"; T.Dollar; T.Ident "x"; T.Ident "y"; T.EOF]);

    (* function composition *)
    ("f . g . h",   [T.Ident "f"; T.Dot; T.Ident "g"; T.Dot; T.Ident "h"; T.EOF]);

    (* mixed *)
    ("f . g $ x <|> y",   [T.Ident "f"; T.Dot; T.Ident "g"; T.Dollar; T.Ident "x"; T.Alt; T.Ident "y"; T.EOF]);

    (* monadic/applicative/alternative *)
    ("m >>= f",     [T.Ident "m"; T.Bind; T.Ident "f"; T.EOF]);
    ("m >> f",      [T.Ident "m"; T.Pipe; T.Ident "f"; T.EOF]);
    ("f <*> x",     [T.Ident "f"; T.UFO; T.Ident "x"; T.EOF]);
    ("f <$> x",     [T.Ident "f"; T.Map; T.Ident "x"; T.EOF]);
    ("m <|> n",     [T.Ident "m"; T.Alt; T.Ident "n"; T.EOF]);

    (* block *)
    ("{ x; y; z }", [T.LCurly; T.Ident "x"; T.Semi; T.Ident "y"; T.Semi; T.Ident "z"; T.RCurly; T.EOF]);

    (* bind *)
    ("let x <- m", [T.Let; T.Ident "x"; T.LArrow; T.Ident "m"; T.EOF]);

  ]
  [@@ocamlformat "disable"]

let binary_suite = List.map test_case_generator binary_cases
let unary_suite = List.map test_case_generator unary_cases
let syntax_suite = List.map test_case_generator syntax_cases

(* Test suite runner *)
let () =
  Alcotest.run "Lexer Tests"
    [
      ("Binary", binary_suite); ("Unary", unary_suite); ("Syntax", syntax_suite);
    ]
