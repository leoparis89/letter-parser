type binary_operator = Plus | Minus | Asterisk
type literal = String of string | Numeric of int

type expression =
  | Literal of literal
  | Binary of expression * binary_operator * expression

type statement =
  | Expression_Statement of expression
  | Block_Statement of statement list

type ast = Program of statement list

type t = {
  mutable lookahead : Tokenizer.production option;
  tokenizer : Tokenizer.t;
}

let make () = { lookahead = None; tokenizer = Tokenizer.make "" }

let get_lookahead_production parser =
  match parser.lookahead with
  | Some production -> production
  | None -> failwith "Unexpected end of input"

let get_lookahead_token parser = (get_lookahead_production parser).token

let eat (token : Tokenizer.token) parser =
  match parser |> get_lookahead_production with
  | production ->
      if not (production.token = token) then
        failwith
          (Printf.sprintf
             "Unexpected token type in lookahead. Found %s but expected %s"
             (Tokenizer.show_token production.token)
             (Tokenizer.show_token token))
      else parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
      production

let numeric_literal parser =
  let token = parser |> eat `Numeric in
  Numeric (int_of_string token.value)

let string_literal parser =
  let token = parser |> eat `String in
  let value = token.value in
  let len = String.length value in
  String (String.sub value 1 (len - 2))

let literal parser =
  match parser |> get_lookahead_token with
  | `String -> string_literal parser
  | `Numeric -> numeric_literal parser
  | token ->
      failwith
        (Printf.sprintf "Literal: unexpected literal production %s"
           (Tokenizer.show_token token))

let token_to_operator : Tokenizer.token -> binary_operator option = function
  | `Plus -> Some Plus
  | `Minus -> Some Minus
  | `Asterisk -> Some Asterisk
  | _ -> None

let rec expression parser =
  let parenthesis_expression parser =
    let _ = parser |> eat `Parenthesis_open in
    let expression = expression parser in
    let _ = parser |> eat `Parenthesis_close in
    expression
  in

  let primary_expression parser =
    match parser.lookahead with
    | Some { token = `Parenthesis_open; _ } -> parenthesis_expression parser
    | _ -> Literal (literal parser)
  in

  let multiplicative_expression parser =
    let left = ref (primary_expression parser) in
    while
      match parser.lookahead with
      | Some { token = `Asterisk; _ } -> true
      | _ -> false
    do
      match parser.lookahead with
      | Some { token; _ } -> (
          match token_to_operator token with
          | Some operator ->
              let _ = parser |> eat token in
              let right = primary_expression parser in
              left := Binary (!left, operator, right)
          | None -> ())
      | None -> ()
    done;
    !left
  in

  let binary_expression parser =
    let left = ref (multiplicative_expression parser) in
    while
      match parser.lookahead with
      | Some { token = `Plus | `Minus; _ } -> true
      | _ -> false
    do
      match parser.lookahead with
      | Some { token; _ } -> (
          match token_to_operator token with
          | Some operator ->
              let _ = parser |> eat token in
              let right = multiplicative_expression parser in
              left := Binary (!left, operator, right)
          | None -> ())
      | None -> ()
    done;
    !left
  in

  binary_expression parser

let expression_statement parser =
  let expression = expression parser in
  let _ = parser |> eat `Semicolon in
  Expression_Statement expression

let rec statement_list ~(stop_lookahead : Tokenizer.token option) parser =
  let block_statement parser =
    parser |> eat `Curly_open |> ignore;
    let body =
      match parser |> get_lookahead_token with
      | `Curly_close -> []
      | _ -> statement_list ~stop_lookahead:(Some `Curly_close) parser
    in
    parser |> eat `Curly_close |> ignore;
    Block_Statement body
  in

  let statement parser =
    match parser |> get_lookahead_token with
    | `Curly_open -> block_statement parser
    | `Numeric | `String | `Parenthesis_open -> expression_statement parser
    | token ->
        failwith
          (Printf.sprintf "Statement: unexpected statement production %s"
             (Tokenizer.show_token token))
  in

  let rec loop acc =
    match (parser.lookahead, stop_lookahead) with
    | None, _ -> List.rev acc
    | Some { token; _ }, Some stop_lookahead when stop_lookahead = token ->
        List.rev acc
    | Some _lookahead, _ ->
        let statement = statement parser in
        loop (statement :: acc)
  in
  loop []

let parse program parser =
  Tokenizer.init program parser.tokenizer;
  parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
  Program (parser |> statement_list ~stop_lookahead:None)
