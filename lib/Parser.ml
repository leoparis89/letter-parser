type literal = String of string | Numeric of int
type expression = Literal of literal | Other_expression
type statement = Expression_Statement of expression | Other_statement
type ast = Program of statement list

type t = {
  mutable _string : string;
  mutable lookahead : Tokenizer.production option;
  tokenizer : Tokenizer.t;
}

let make () = { _string = ""; lookahead = None; tokenizer = Tokenizer.make "" }

let eat (token : Tokenizer.token) parser =
  match parser.lookahead with
  | Some production ->
      if not (production.token = token) then failwith "Unexpected token"
      else parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
      production
  (* failwith "Unexpected token" *)
  | None -> failwith "Unexpected end of input"

let numeric_literal parser =
  let token = parser |> eat Numeric in
  Numeric (int_of_string token.value)

let string_literal parser =
  let token = parser |> eat String in
  let value = token.value in
  let len = String.length value in
  String (String.sub value 1 (len - 2))

let literal parser =
  match parser.lookahead with
  | None -> failwith "Empty parser"
  | Some { token = String; _ } -> string_literal parser
  | Some { token = Numeric; _ } -> numeric_literal parser
  | Some { token = Semicolon; _ } -> failwith "Unexpected token"

let expression parser = Literal (literal parser)

let expression_statement parser =
  let expression = expression parser in
  let _ = parser |> eat Semicolon in
  Expression_Statement expression

let statement parser = expression_statement parser

let statement_list parser =
  let rec loop acc =
    if parser.lookahead = None then List.rev acc
    else
      let statement = statement parser in
      loop (statement :: acc)
  in
  loop []

let program parser =
  let rec loop acc =
    if parser.lookahead = None then List.rev acc
    else
      let literal = literal parser in
      loop (literal :: acc)
  in
  loop []

let parse program parser =
  parser._string <- program;
  Tokenizer.init parser._string parser.tokenizer;
  parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
  Program (statement_list parser)
