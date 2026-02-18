module T = Lexer.Token

type 'a parser_result = 
  | Ok of 'a
  | Error of string

type parse_state = {
  tokens: Lexer.Token.t list;
  mutable pos: int;
}

(* Monad to allow sequencing of actions within a Result type *)
let ( let* ) result f = 
  match result with
  | Ok x -> f x
  | Error e -> Error e

let create_parse_state tokens = {
  tokens;
  pos = 0;
}

(* Parser primitives *)
let peek state = 
  List.nth_opt state.tokens state.pos

let advance state =
  match List.nth_opt state.tokens state.pos with
  | Some token -> 
      state.pos <- state.pos + 1;
      Some token
  | None -> None

let expect state e_token = 
  match peek state with
  | Some token when token = e_token -> 
      state.pos <- state.pos + 1;
      Ok token
  | Some token -> 
      Error (Printf.sprintf "%d: Expected token %s but found %s" 
        state.pos
        (Lexer.string_of_token e_token)
        (Lexer.string_of_token token))
  | None -> 
      Error (Printf.sprintf "%d: Expected token %s but found end of input" 
        state.pos
        (Lexer.string_of_token e_token))

let expect_ident state = 
  match peek state with
  | Some (T.Ident id) -> 
      state.pos <- state.pos + 1;
      Ok id
  | Some token ->
      Error (Printf.sprintf "%d: Expected identifier but found %s" 
        state.pos
        (Lexer.string_of_token token))
  | None ->
      Error (Printf.sprintf "%d: Expected identifier but found end of input" 
        state.pos)

(* Parsing *)


(* Pratt parser: precedence climber *)
(*
    The binding power of a token determines which direction it binds.
    In the case of 1 + 2, the '+' operator binds more strongly to the right.
*)

(* Prefix binding power: negation or not *)
(* Default binding power for prefix operators is 10 *)
let prefix_bp = 10

(* Function application has highest precedence *)
let app_bp = 20
let compose_bp = 18
let dollar_bp = 10

(* (left_bp, right_bp) *)
let infix_bp = function
  | T.Plus | T.Minus -> (1, 2)
  | T.Star | T.Slash | T.Percent -> (3, 4)

  (* Non-associative *)
  | T.Eq | T.NotEq | T.Less | T.LessEq | T.Greater | T.GreaterEq -> (5, 5)

  (* function application: f $ x y *)
  | T.Dollar -> (dollar_bp, dollar_bp - 1) (* 10, 9 - right assoc *)

  (* function composition: f . g *)
  | T.Dot -> (compose_bp, compose_bp - 1) (* 18, 17 - right assoc *)

  | _ -> (0, 0)

let token_to_binop  = function
  | T.Plus -> Some Ast.BinOp.Add
  | T.Minus -> Some Ast.BinOp.Sub
  | T.Star -> Some Ast.BinOp.Mul
  | T.Slash -> Some Ast.BinOp.Div
  | T.Percent -> Some Ast.BinOp.Mod
  | T.Eq -> Some Ast.BinOp.Eq
  | T.NotEq -> Some Ast.BinOp.NotEq
  | T.Less -> Some Ast.BinOp.Less
  | T.LessEq -> Some Ast.BinOp.LessEq
  | T.Greater -> Some Ast.BinOp.Greater
  | T.GreaterEq -> Some Ast.BinOp.GreaterEq
  | T.Dollar -> Some Ast.BinOp.Apply
  | T.Dot -> Some Ast.BinOp.Compose
  | _ -> None

let classify_type pstate ty_token = 
  match ty_token with
  | T.Ident "Int" -> Ok Ast.Ty.Int
  | T.Ident "Float" -> Ok Ast.Ty.Float
  | T.Ident "Bool" -> Ok Ast.Ty.Bool
  | T.Ident "String" -> Ok Ast.Ty.String
  | T.Ident name -> Ok (Ast.Ty.TyUser name)
  | token -> Error (Printf.sprintf "%d: Expected type but found %s"
      pstate.pos
      (Lexer.string_of_token token))

let can_start_arg = function
  | T.Number _ | T.String _ | T.Ident _ | T.LParen -> true
  | _ -> false

let rec parse_atom pstate : Ast.Expr.t parser_result = 
  match peek pstate with
  | Some (T.Number _ as token) -> 
      advance pstate |> ignore;
      Ok (Ast.Expr.Literal token)
  | Some (T.String _ as token) -> 
      advance pstate |> ignore;
      Ok (Ast.Expr.Literal token)
  | Some (T.Ident id) -> 
      advance pstate |> ignore;
      Ok (Ast.Expr.Ident id)
  | Some T.LParen -> 
      advance pstate |> ignore;
      let* rhs = parse_expr_bp pstate 0 in
      let* _ = expect pstate T.RParen in
      Ok rhs
  | Some token ->
      Error (Printf.sprintf "%d: Unexpected token %s"
        pstate.pos
        (Lexer.string_of_token token))
  | None ->
      Error (Printf.sprintf "%d: Unexpected end of input" pstate.pos)

(* Parse an expression with a given minimum binding power *)
and parse_expr_bp pstate min_bp : Ast.Expr.t parser_result =
  let* lhs = parse_atom pstate in 
  let rec loop lhs = 
    match peek pstate with
    | Some op ->
        if can_start_arg op && app_bp >= min_bp then
          (* function application *)
          let* arg = parse_expr_bp pstate (app_bp + 1) in 
          loop (Ast.Expr.App (lhs, arg))
        else
          (* infix operator *)
          let (l_bp, r_bp) = infix_bp op in

          (* if the precedence is less than min_bp, stop and return lhs *)
          if l_bp < min_bp then Ok lhs
          else
            (* otherwise, consume and recursive parse the rhs *)
            (match token_to_binop op with
            | Some binop ->
                advance pstate |> ignore;
                let* rhs = parse_expr_bp pstate r_bp in
                loop (Ast.Expr.Binary (binop, lhs, rhs))
            | None -> Ok lhs)
    | None -> Ok lhs
  in
  loop lhs

let rec parse_expr pstate : Ast.Expr.t parser_result = 
  parse_expr_bp pstate 0

let parse_type pstate : Ast.Ty.t parser_result = 
  match peek pstate with
  | Some ty_token ->
      let* ty = classify_type pstate ty_token in
      advance pstate |> ignore;
      Ok ty
  | None ->
      Error (Printf.sprintf "%d: Expected type but found end of input" pstate.pos)

let parse_let pstate : Ast.Expr.t parser_result = 
  (* let ident: <type> = <expr> *)
  advance pstate |> ignore; (* consume 'let' *)
  let* id = expect_ident pstate in
  let* ty_opt = 
    match peek pstate with
    | Some T.Colon ->
        advance pstate |> ignore; (* consume ':' *)
        let* ty = parse_type pstate in
        Ok (Some ty)
    | _ -> Ok None
  in
  let* _ = expect pstate T.Eq in (* consume '=' *)
  let* expr = parse_expr pstate in
  Ok (Ast.Expr.Let (id, ty_opt, expr))

let parse_if pstate : Ast.Expr.t parser_result =
  (* if cond then <then_branch> else <else_branch> *)
  advance pstate |> ignore; (* consume if *)
  let* cond = parse_expr pstate in
  let* _ = expect pstate T.Then in (* consume then *)
  let* then_branch = parse_expr pstate in
  let* _ = expect pstate T.Else in (* consume else *)
  let* else_branch = parse_expr pstate in
  Ok (Ast.Expr.If (cond, then_branch, Some else_branch))

let parse_lam pstate : Ast.Expr.t parser_result =
  (* \x y -> body *)
  advance pstate |> ignore; (* consume '\' *)

  (* recursive function that parses parameters *)
  let rec parse_params acc = 
    match peek pstate with
    | Some (T.Ident id) ->
        advance pstate |> ignore;
        parse_params (id :: acc)
    | Some T.Arrow ->
        advance pstate |> ignore; (* consume '->' *)
        Ok (List.rev acc)
    | Some token ->
        Error (Printf.sprintf "%d: Expected parameter name or '->' but found %s"
          pstate.pos
          (Lexer.string_of_token token))
    | None ->
        Error (Printf.sprintf "%d: Expected parameter name or '->' but found end of input" pstate.pos)
  in
  let* params = parse_params [] in
  let* body = parse_expr pstate in
  Ok (Ast.Expr.Lam (params, body))

let parse_block pstate : Ast.Expr.t parser_result =
  (* monadic block - { e1; e2; e3 } *)
  advance pstate |> ignore; (* consume '{' *)
  failwith "parse_block not implemented yet"

let parse_bind pstate : Ast.Expr.t parser_result =
  (* let x <- m *)
  advance pstate |> ignore; (* consume 'let' *)
  let* id = expect_ident pstate in
  let* _ = expect pstate T.Bind in (* consume '<-' *)
  let* expr = parse_expr pstate in
  Ok (Ast.Expr.Bind (id, expr))