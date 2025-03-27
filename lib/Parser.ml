type binary_operator = Plus | Minus
type literal = String of string | Numeric of int

type expression =
  | Literal of literal
  | Binary of expression * binary_operator * expression

let get_numeric_literal = function
  | Literal (Numeric literal) -> literal
  | _ -> failwith "Not a literal"

type statement =
  | Expression_Statement of expression
  | Block_Statement of statement list

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
      if not (production.token = token) then
        failwith
          (Printf.sprintf
             "Unexpected token type in lookahead. Found %s but expected %s"
             (Tokenizer.show_token production.token)
             (Tokenizer.show_token token))
      else parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
      production
  (* failwith "Unexpected token" *)
  | None -> failwith "Empty parser"

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
  | Some { token; _ } ->
      failwith
        (Printf.sprintf
           "Unexpected token type in lookahead. Found %s but expected String \
            or Numeric"
           (Tokenizer.show_token token))

let token_to_operator : Tokenizer.token -> binary_operator option = function
  | Plus -> Some Plus
  | Minus -> Some Minus
  | _ -> None

let binary_expression parser =
  let left = ref (Literal (literal parser)) in
  while
    match parser.lookahead with
    | Some { token = Plus | Minus; _ } -> true
    | _ -> false
  do
    match parser.lookahead with
    | Some { token; _ } -> (
        match token_to_operator token with
        | Some operator ->
            let _ = parser |> eat token in
            let right = Literal (literal parser) in
            left := Binary (!left, operator, right)
        | None -> ())
    | None -> ()
  done;
  !left

let expression = binary_expression

let expression_statement parser =
  let expression = expression parser in
  let _ = parser |> eat Semicolon in
  Expression_Statement expression

let rec statement_list ~(stop_lookahead : Tokenizer.token option) parser =
  let block_statement parser =
    let _ = parser |> eat Curly_open in
    let body =
      match parser.lookahead with
      | Some { token = Curly_close; _ } -> []
      | Some _ -> statement_list ~stop_lookahead:(Some Curly_close) parser
      | None -> failwith "Unexpected empty lookahead"
    in
    let _ = parser |> eat Curly_close in
    Block_Statement body
  in

  let statement parser =
    match parser.lookahead with
    | Some { token = Curly_open; _ } -> block_statement parser
    | Some { token = Numeric | String; _ } -> expression_statement parser
    | Some { token; _ } ->
        failwith
          (Printf.sprintf
             "Unexpected token type in lookahead while parsing statement. \
              Found %s but expected Curly_open, Numeric, or String"
             (Tokenizer.show_token token))
    | None -> failwith "Unexpected empty lookahead"
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
  parser._string <- program;
  Tokenizer.init parser._string parser.tokenizer;
  parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
  Program (parser |> statement_list ~stop_lookahead:None)
