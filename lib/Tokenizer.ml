module Regex = struct
  let re_pattern str = Re.compile (Re.Perl.re str)

  let match_regex pattern str =
    try Some (Re.exec pattern str) with Not_found -> None

  let get_capture group match_result =
    try Some (Re.Group.get match_result group) with Not_found -> None

  let match_and_capture pattern str =
    match match_regex pattern str with
    | Some result -> (
        match get_capture 0 result with
        | Some captured -> Some captured
        | None -> None)
    | None -> None
end

type token =
  [ `Numeric
  | `String
  | `Semicolon
  | `Curly_open
  | `Curly_close
  | `Plus
  | `Minus
  | `Asterisk
  | `Parenthesis_open
  | `Parenthesis_close ]
[@@deriving show]

type spec = { pattern : string; token : token option }

let specs =
  [
    (* match asterisk *)
    { pattern = "^\\*"; token = Some `Asterisk };
    (* match plus *)
    { pattern = "^\\+"; token = Some `Plus };
    (* match minus *)
    { pattern = "^\\-"; token = Some `Minus };
    (* match parenthesis open *)
    { pattern = "^\\("; token = Some `Parenthesis_open };
    (* match parenthesis close *)
    { pattern = "^\\)"; token = Some `Parenthesis_close };
    (* left curly brace *)
    { pattern = "^\\{"; token = Some `Curly_open };
    (* right curly brace *)
    { pattern = "^\\}"; token = Some `Curly_close };
    (* match multi line comments *)
    { pattern = "^/\\*[\\s\\S]*?\\*/"; token = None };
    (* skip single line comments *)
    { pattern = "^//.*"; token = None };
    (* skip whitespace *)
    { pattern = "^\\s+"; token = None };
    (* match_semicolon *)
    { pattern = "^;"; token = Some `Semicolon };
    (* match strings *)
    { pattern = "^\"[^\"]+\""; token = Some `String };
    (* match numbers *)
    { pattern = "^\\d+"; token = Some `Numeric };
  ]

type t = { mutable _string : string; mutable cursor : int }
type production = { token : token; value : string }

let is_EOF t = t.cursor >= String.length t._string
let has_more_tokens t = t.cursor < String.length t._string

let match_token_pattern pattern str (t : t) =
  match Regex.match_and_capture pattern str with
  | Some result ->
      t.cursor <- t.cursor + String.length result;
      Some result
  | None -> None

let get_remaining_string t =
  String.sub t._string t.cursor (String.length t._string - t.cursor)

let rec get_next_token tokenizer (token_matchers : spec list) =
  if not (has_more_tokens tokenizer) then None
  else
    let rec loop t (current_specs : spec list) str =
      match current_specs with
      | [] ->
          failwith (Printf.sprintf "Unexpected token: %c" (String.get str 0))
      | { pattern; token } :: remaining_specs -> (
          Printf.printf "looping %s %s\n" str pattern;
          let matched_string =
            match_token_pattern (Regex.re_pattern pattern) str t
          in
          match (token, matched_string) with
          | None, Some _token_to_ignore ->
              Printf.printf "ignoring pattern %s %s\n" pattern _token_to_ignore;
              get_next_token t specs
          | Some token, Some value ->
              Printf.printf "found token %s\n" value;
              Some { token; value }
          | _, None -> loop t remaining_specs str)
    in
    let remaining_string = get_remaining_string tokenizer in
    loop tokenizer token_matchers remaining_string

let get_next_token tokenizer = get_next_token tokenizer specs
let make _string = { _string; cursor = 0 }

let init string tokenizer =
  tokenizer._string <- string;
  tokenizer.cursor <- 0
