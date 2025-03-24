type literal = String of string | Numeric of int

(* type statement = Expression of literal *)
type ast = Program of literal list

type t = {
  mutable _string : string;
  mutable lookahead : Tokenizer.token_value option;
  tokenizer : Tokenizer.t;
}

let make () = { _string = ""; lookahead = None; tokenizer = Tokenizer.make "" }
let program _t = Program []

let numeric_literal parser =
  (* let token = eat parser in *)
  Numeric (int_of_string parser._string)

(* let string_literal parser =
  let token = eat parser in
  String token *)
(* let expression_statement (_parser : t) : statement = Expression (String "mock")
let statement parser = expression_statement parser *)

(* let statement_list parser =
  let rec loop acc =
    if parser.lookahead = None then List.rev acc
    else
      let statement = statement parser in
      loop (statement :: acc)
  in
  loop [ statement parser ] *)

let eat parser =
  let token = parser.lookahead in
  parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
  token

let parse program parser =
  parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
  parser._string <- program;
  Tokenizer.init parser._string parser.tokenizer;
  Program [ numeric_literal parser ]
