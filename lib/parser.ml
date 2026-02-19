module T = Lexer.Token

type 'a parser_result = Ok of 'a | Error of string
type parse_state = { tokens : Lexer.Token.t list; mutable pos : int }

(* Monad to allow sequencing of actions within a Result type *)
let ( let* ) result f =
  match result with
  | Ok x -> f x
  | Error e -> Error e

let create_parse_state tokens = { tokens; pos = 0 }

(* Parser primitives *)
let peek state = List.nth_opt state.tokens state.pos

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
      Error
        (Printf.sprintf "%d: Expected token %s but found %s" state.pos
           (Lexer.string_of_token e_token)
           (Lexer.string_of_token token))
  | None ->
      Error
        (Printf.sprintf "%d: Expected token %s but found end of input" state.pos
           (Lexer.string_of_token e_token))

let expect_ident state =
  match peek state with
  | Some (T.Ident id) ->
      state.pos <- state.pos + 1;
      Ok id
  | Some token ->
      Error
        (Printf.sprintf "%d: Expected identifier but found %s" state.pos
           (Lexer.string_of_token token))
  | None ->
      Error
        (Printf.sprintf "%d: Expected identifier but found end of input"
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

  (* Monadic operators, right assoc, lower precedence *)
  | T.Alt -> (6, 5)   (* <|> - alternative, lowest *)
  | T.Bind -> (8, 7)  (* >>= - monadic bind *)
  | T.Pipe -> (7, 6)  (* >>  - monadic sequencing *)
  | T.UFO -> (9, 8)   (* <*> - applicative functor *)
  | T.Map -> (9, 8)   (* <$> - functor map *)

  (* function application: f $ x y *)
  | T.Dollar -> (dollar_bp, dollar_bp - 1) (* 10, 9 - right assoc *)

  (* function composition: f . g *)
  | T.Dot -> (compose_bp, compose_bp - 1) (* 18, 17 - right assoc *)

  | _ -> (0, 0)
  [@@ocamlformat "disable"]

let token_to_binop = function
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

  (* Monadic/applicative/alternative *)
  | T.Bind -> Some Ast.BinOp.Bind
  | T.Pipe -> Some Ast.BinOp.Pipe
  | T.UFO -> Some Ast.BinOp.UFO
  | T.Map -> Some Ast.BinOp.Map
  | T.Alt -> Some Ast.BinOp.Alt
  | _ -> None
  [@@ocamlformat "disable"]

let can_start_arg = function
  | T.Number _
  | T.String _
  | T.Ident _
  | T.LParen ->
      true
  | _ -> false

(* Parse a single atomic expression *)
let rec parse_atom pstate : Ast.Expr.t parser_result =
  let parse_group_like ps =
    (* lookahead to check for unit (), tuple, or grouped expression *)
    let saved_pos = ps.pos in
    advance ps |> ignore;
    match peek ps with
    | Some T.RParen ->
        (* unit: () *)
        advance ps |> ignore;
        Ok Ast.Expr.Unit
    | Some _ -> (
        (* parse first element *)
        let* first = parse_expr_bp ps 0 in
        match peek ps with
        | Some T.Comma ->
            (* tuple: (e1, e2, ...) *)
            ps.pos <- saved_pos;
            (* reset position *)
            parse_tuple ps
        | Some T.RParen ->
            (* grouped expression: (e) *)
            advance ps |> ignore;
            Ok first
        | Some token ->
            Error
              (Printf.sprintf "%d: Expected ',' or ')' but found %s" ps.pos
                 (Lexer.string_of_token token))
        | None ->
            Error
              (Printf.sprintf "%d: Expected ')' but found end of input" ps.pos))
    | None ->
        Error
          (Printf.sprintf
             "%d: Expected expression or ')' but found end of input" ps.pos)
  in

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
  (* keywords *)
  | Some T.Let -> parse_let pstate
  | Some T.If -> parse_if pstate
  | Some T.Loop -> parse_loop pstate
  (* special cases *)
  | Some T.Backslash -> parse_lam pstate
  | Some T.LBrack -> parse_list pstate
  | Some T.LCurly -> parse_block pstate
  | Some T.LParen -> parse_group_like pstate
  (* end special cases *)
  | Some token ->
      Error
        (Printf.sprintf "%d: Unexpected token %s" pstate.pos
           (Lexer.string_of_token token))
  | None -> Error (Printf.sprintf "%d: Unexpected end of input" pstate.pos)

(* Parse an expression with a given minimum binding power *)
and parse_expr_bp pstate min_bp : Ast.Expr.t parser_result =
  let* lhs = parse_atom pstate in
  let rec loop lhs =
    match peek pstate with
    | Some op -> (
        if can_start_arg op && app_bp >= min_bp then
          (* function application *)
          let* arg = parse_expr_bp pstate (app_bp + 1) in
          loop (Ast.Expr.App (lhs, arg))
        else
          (* infix operator *)
          let l_bp, r_bp = infix_bp op in

          (* if the precedence is less than min_bp, stop and return lhs *)
          if l_bp < min_bp then Ok lhs
          else
            (* otherwise, consume and recursive parse the rhs *)
            match token_to_binop op with
            | Some binop ->
                advance pstate |> ignore;
                let* rhs = parse_expr_bp pstate r_bp in
                loop (Ast.Expr.Binary (binop, lhs, rhs))
            | None -> Ok lhs)
    | None -> Ok lhs
  in
  loop lhs

and parse_expr pstate : Ast.Expr.t parser_result = parse_expr_bp pstate 0

and parse_enclosed pstate o s c f ctor =
  let* _ = expect pstate o in
  let rec parse_elements acc =
    match peek pstate with
    | Some token when token = c ->
        (* end of enclosed *)
        advance pstate |> ignore;
        Ok (ctor (List.rev acc))
    | Some _ -> (
        (* parse enclosed item *)
        let* elem = f pstate in
        match peek pstate with
        | Some token when token = s ->
            advance pstate |> ignore;
            parse_elements (elem :: acc)
        | Some token when token = c ->
            advance pstate |> ignore;
            Ok (ctor (List.rev (elem :: acc)))
        | Some token ->
            Error
              (Printf.sprintf "%d: Expected %s or %s but found %s" pstate.pos
                 (Lexer.string_of_token o) (Lexer.string_of_token c)
                 (Lexer.string_of_token token))
        | None ->
            Error
              (Printf.sprintf "%d: Expected %s or %s but found end of input"
                 pstate.pos (Lexer.string_of_token o) (Lexer.string_of_token c))
        )
    | None ->
        (* end of input, invalid state *)
        Error
          (Printf.sprintf "%d: Expected %s or %s but found end of input"
             pstate.pos (Lexer.string_of_token o) (Lexer.string_of_token c))
  in
  parse_elements []

and parse_list pstate : Ast.Expr.t parser_result =
  (* [e1, e2, e3] *)
  parse_enclosed pstate T.LBrack T.Comma T.RBrack parse_expr (fun es ->
      Ast.Expr.List es)

and parse_tuple pstate : Ast.Expr.t parser_result =
  (* (e1, e2, e3) *)
  parse_enclosed pstate T.LParen T.Comma T.RParen parse_expr (fun es ->
      match es with
      | [ e ] -> e (* single element tuple is just the element *)
      | _ -> Ast.Expr.Tuple es)

and parse_type pstate : Ast.Ty.t parser_result =
  let* base_ty = parse_type_atom pstate in
  (* Check for function type arrow *)
  match peek pstate with
  | Some T.Arrow ->
      advance pstate |> ignore;
      let rec parse_fn_types acc =
        let* ty = parse_type_atom pstate in
        (* Check for type application on this argument *)
        let* ty_with_app = parse_type_app pstate ty in
        match peek pstate with
        | Some T.Arrow ->
            advance pstate |> ignore;
            parse_fn_types (ty_with_app :: acc)
        | _ ->
            (* Last type is the return type *)
            Ok (Ast.Ty.Fn (List.rev acc, ty_with_app))
      in
      parse_fn_types [ base_ty ]
  | _ ->
      (* Try to parse type application *)
      parse_type_app pstate base_ty

and parse_type_atom pstate : Ast.Ty.t parser_result =
  (* Int, Float, Bool, String, Unit, or user-defined types *)
  match peek pstate with
  | Some T.Unit ->
      advance pstate |> ignore;
      Ok Ast.Ty.Unit
  | Some (T.Ident name) ->
      advance pstate |> ignore;
      Ok
        (match name with
        | "Int" -> Ast.Ty.Int
        | "Float" -> Ast.Ty.Float
        | "Bool" -> Ast.Ty.Bool
        | "String" -> Ast.Ty.String
        | _ -> Ast.Ty.TyUser name)
  | Some token ->
      Error
        (Printf.sprintf "%d: Expected type but found %s" pstate.pos
           (Lexer.string_of_token token))
  | None ->
      Error
        (Printf.sprintf "%d: Expected type but found end of input" pstate.pos)

and parse_type_app pstate base_ty : Ast.Ty.t parser_result =
  (* Maybe Int, A -> B -> C *)
  let rec loop ty args =
    match peek pstate with
    | Some (T.Ident _)
    | Some T.Unit ->
        (* parse next argument *)
        let* arg = parse_type_atom pstate in
        loop ty (arg :: args)
    | Some T.Arrow -> (
        (* stop here - arrow will be handled by parse_type *)
        match List.rev args with
        | [] -> Ok ty
        | args ->
            let name =
              match ty with
              | Ast.Ty.TyUser name -> name
              | Ast.Ty.Int -> "Int"
              | Ast.Ty.Float -> "Float"
              | Ast.Ty.Bool -> "Bool"
              | Ast.Ty.String -> "String"
              | _ -> failwith "Unexpected base type in type application"
            in
            Ok (Ast.Ty.TyCons (name, args)))
    | _ -> (
        (* no more arguments *)
        match List.rev args with
        | [] -> Ok ty
        | args ->
            let name =
              match ty with
              | Ast.Ty.TyUser name -> name
              | Ast.Ty.Int -> "Int"
              | Ast.Ty.Float -> "Float"
              | Ast.Ty.Bool -> "Bool"
              | Ast.Ty.String -> "String"
              | _ -> failwith "Unexpected base type in type application"
            in
            Ok (Ast.Ty.TyCons (name, args)))
  in
  loop base_ty []

and parse_let pstate : Ast.Expr.t parser_result =
  (* not sure if this should be let rec or not *)

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

  (* check for in or where clause *)
    match peek pstate with
    | Some T.In ->
        advance pstate |> ignore; (* consume 'in' *)
        let* body = parse_expr pstate in
        Ok (Ast.Expr.LetIn (id, ty_opt, expr, body))
    | Some T.Where ->
        advance pstate |> ignore; (* consume 'where' *)
        let* clauses = parse_where_clauses pstate in
        Ok (Ast.Expr.LetWhere (id, ty_opt, expr, clauses))
    | _ ->

  Ok (Ast.Expr.Let (id, ty_opt, expr))

  [@@ocamlformat "disable"]

and parse_where_clauses pstate : Ast.Clause.t list parser_result =
  (* let result = expr where x = 10 *)
  let parse_clause ps = 
    advance ps |> ignore; (* consume 'where' *)
    let* id = expect_ident ps in
    let* ty_opt = 
      match peek ps with
      | Some T.Colon ->
          advance ps |> ignore; (* consume ':' *)
          let* ty = parse_type ps in
          Ok (Some ty)
      | _ -> Ok None
    in
    let* _ = expect ps T.Eq in (* consume '=' *)
    let* expr = parse_expr ps in
    Ok (Ast.Clause.Where (id, ty_opt, expr))
  in

  (* parse many where clauses *)
  let rec loop acc =
    match peek pstate with
    | Some T.Where ->
        let* clause = parse_clause pstate in
        loop (clause :: acc)
    | _ -> Ok (List.rev acc)
  in
  loop []

and parse_if pstate : Ast.Expr.t parser_result =
  (* if cond then <then_branch> else <else_branch> *)
  advance pstate |> ignore; (* consume if *)
  let* cond = parse_expr pstate in
  let* _ = expect pstate T.Then in (* consume then *)
  let* then_branch = parse_expr pstate in
  let* _ = expect pstate T.Else in (* consume else *)
  let* else_branch = parse_expr pstate in
  Ok (Ast.Expr.If (cond, then_branch, Some else_branch))

  [@@ocamlformat "disable"]

and parse_lam pstate : Ast.Expr.t parser_result =
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

  [@@ocamlformat "disable"]

and parse_loop pstate : Ast.Expr.t parser_result =
  (* loop (<optional condition>) body *)
  advance pstate |> ignore; (* consume 'loop' *)
  let* cond_opt = 
    match peek pstate with
    | Some T.LParen ->
        advance pstate |> ignore; (* consume '(' *)
        let* cond = parse_expr pstate in
        let* _ = expect pstate T.RParen in (* consume ')' *)
        Ok (Some cond)
    | _ -> Ok None
  in
  let* body = parse_expr pstate in
  Ok (Ast.Expr.Loop (cond_opt, body))

  [@@ocamlformat "disable"]

and parse_block pstate : Ast.Expr.t parser_result =
  (* monadic block: { e1; e2; e3 } *)
  advance pstate |> ignore; (* consume '{' *)
  failwith "parse_block not implemented yet"

  [@@ocamlformat "disable"]

and parse_bind pstate : Ast.Expr.t parser_result =
  (* let x <- m *)
  advance pstate |> ignore; (* consume 'let' *)
  let* id = expect_ident pstate in
  let* _ = expect pstate T.Bind in (* consume '<-' *)
  let* expr = parse_expr pstate in
  Ok (Ast.Expr.Bind (id, expr))

  [@@ocamlformat "disable"]
