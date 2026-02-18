(* Internal DFA state *)
module DFA = struct
  type t = 
    | Start
    | Letter
    | Digit
    | Operator
    | Error
end

module CharClass = struct
  type t = 
    | Ident
    | Digit
    | Quote
    | Operator
    | WS
    | EOF
    | Other
end

module Token = struct
  type t = 
    | Ident of string
    | Number of int
    | String of string
    | Plus
    | Minus
    | Star
    | Slash
    | Percent
    | Dot
    | Comma
    | Colon
    | Semi
    | Arrow     (* -> *)
    | Bind      (* <- *)
    | Backslash (* \ *)
    | Dollar    (* $ *)
    | LParen
    | RParen
    | LBrack
    | RBrack
    | LCurly
    | RCurly
    | Eq
    | EqEq
    | NotEq
    | Less
    | LessEq
    | Greater
    | GreaterEq

    (* Keywords *)
    | Let
    | Fn
    | If
    | Then
    | Else
    | For
    | Loop
    | Match
    | Return

    | EOF
end

(* Imperative core, functional shell *)
type lex_state = {
  input: string;
  mutable pos: int;
  length: int;
}

let create_lex_state s = {
  input = s;
  pos = 0;
  length = String.length s;
}

let peek lex = 
  if lex.pos >= lex.length then None
  else Some (String.get lex.input lex.pos)

let peek_next lex = 
  if lex.pos + 1 >= lex.length then None
  else Some (String.get lex.input (lex.pos + 1))
    
let advance lex = 
  let c = peek lex in
    if c <> None then lex.pos <- lex.pos + 1;
    c

(* Helper functions for DFA *)
let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_ws = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let is_operator_char = function
  | '+' | '-' | '*' | '/' | '%' | '$' 
  | '.' | ',' | '(' | ')' | '[' 
  | ']' | '{' | '}' | '=' | '!' 
  | '<' | '>' | ':' | ';' | '\\' -> true
  | _ -> false

let char_class_of = function
  | c when is_alpha c -> CharClass.Ident
  | c when is_digit c -> CharClass.Digit
  | '"'               -> CharClass.Quote
  | c when is_operator_char c -> CharClass.Operator
  | c when is_ws c    -> CharClass.WS
  | '\000'            -> CharClass.EOF
  | _                 -> CharClass.Other

(* DFA state transition function *)
(*
    Σ is the set of input symbols for the DFA (character classes)
    Q is the set of states (DFA.Start, DFA.Letter, DFA.Digit, DFA.Operator, DFA.Error)
    δ: Q × Σ → Q is the transition function defined by next_state
    q0 is the start state (DFA.Start)
*)

let next_state dfa c_class = 
  match (dfa, c_class) with
  (* transition to an accepting state based on the transition function *)
  | (DFA.Start, CharClass.Ident) -> DFA.Letter
  | (DFA.Start, CharClass.Digit) -> DFA.Digit
  | (DFA.Start, CharClass.Operator) -> DFA.Operator
  | (DFA.Operator, CharClass.Operator) -> DFA.Operator (* allow multi-char operators *)
  | (DFA.Letter, CharClass.Ident) | (DFA.Letter, CharClass.Digit) -> DFA.Letter
  | (DFA.Digit, CharClass.Digit) -> DFA.Digit
  | _ -> DFA.Error

let rec consume_token lex dfa_state lexeme = 
  match peek lex with
  | None -> (dfa_state, lexeme) (* End of input, return current state *)
  | Some c -> 
      let c_class = char_class_of c in
      let next_DFA = next_state dfa_state c_class in
      match next_DFA with
      | DFA.Error -> (dfa_state, lexeme) (* Invalid transition, return current token *)
      | _ ->
        (* accept the character and continue consuming *)
        let _ = advance lex in
        consume_token lex next_DFA (lexeme ^ String.make 1 c)

let classify_operator op = 
  match op with
  | "+" -> Token.Plus
  | "-" -> Token.Minus
  | "*" -> Token.Star
  | "/" -> Token.Slash
  | "%" -> Token.Percent
  | "." -> Token.Dot
  | "," -> Token.Comma
  | "(" -> Token.LParen
  | ")" -> Token.RParen
  | "[" -> Token.LBrack
  | "]" -> Token.RBrack
  | "{" -> Token.LCurly
  | "}" -> Token.RCurly
  | ":" -> Token.Colon
  | ";" -> Token.Semi
  | "\\" -> Token.Backslash
  | "$" -> Token.Dollar
  | "->" -> Token.Arrow
  | "<-" -> Token.Bind
  | "=" -> Token.Eq
  | "==" -> Token.EqEq
  | "!=" -> Token.NotEq
  | "<" -> Token.Less
  | "<=" -> Token.LessEq
  | ">" -> Token.Greater
  | ">=" -> Token.GreaterEq
  | _ -> failwith ("Unknown operator: " ^ op)

let classify_keyword maybe_kw = 
  match maybe_kw with
  | "let" -> Token.Let
  | "fn" -> Token.Fn
  | "if" -> Token.If
  | "then" -> Token.Then
  | "else" -> Token.Else
  | "for" -> Token.For
  | "loop" -> Token.Loop
  | "match" -> Token.Match
  | "return" -> Token.Return
  | _ -> Token.Ident maybe_kw

let tokenize input = 
  let state = create_lex_state input in
  let rec loop acc = 
    match peek state with
    | None -> List.rev (Token.EOF :: acc) (* End of input token *)
    | Some c when is_ws c -> 
      let _ = advance state in loop acc 
    | Some _ -> 
      let (final_state, lexeme) = consume_token state DFA.Start "" in

      (* Map DFA state to token type *)
      let t_type = match final_state with
        | DFA.Letter -> classify_keyword lexeme
        | DFA.Digit ->  Token.Number (int_of_string lexeme)
        | DFA.Operator -> classify_operator lexeme
        | _ -> failwith ("Unexpected token: " ^ lexeme)
      in
      loop (t_type :: acc)
  in
  loop []

(* Testing *)
let string_of_token = function 
  | Token.Ident s -> "Ident(" ^ s ^ ")"
  | Token.Number n -> "Number(" ^ string_of_int n ^ ")"
  | Token.String s -> "String(" ^ s ^ ")"
  | Token.Plus -> "Plus"
  | Token.Minus -> "Minus"
  | Token.Star -> "Star"
  | Token.Slash -> "Slash"
  | Token.Percent -> "Percent"
  | Token.Dot -> "Dot"
  | Token.Comma -> "Comma"
  | Token.Colon -> "Colon"
  | Token.Semi -> "Semi"
  | Token.Arrow -> "Arrow"
  | Token.Bind -> "Bind"
  | Token.Backslash -> "Backslash"
  | Token.Dollar -> "Dollar"
  | Token.LParen -> "LParen"
  | Token.RParen -> "RParen"
  | Token.LBrack -> "LBrack"
  | Token.RBrack -> "RBrack"
  | Token.LCurly -> "LCurly"
  | Token.RCurly -> "RCurly"

  | Token.Eq -> "Eq"
  | Token.EqEq -> "EqEq"
  | Token.NotEq -> "NotEq"
  | Token.Less -> "Less"
  | Token.LessEq -> "LessEq"
  | Token.Greater -> "Greater"
  | Token.GreaterEq -> "GreaterEq"

    (* Keywords *)
  | Token.Let -> "let"
  | Token.Fn -> "fn"
  | Token.If -> "if"
  | Token.Then -> "then"
  | Token.Else -> "else"
  | Token.For -> "for"
  | Token.Loop -> "loop"
  | Token.Match -> "match"
  | Token.Return -> "return"
  | Token.EOF -> "EOF"
  (* | _ -> "Unknown" *)