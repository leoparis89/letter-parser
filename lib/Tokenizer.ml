module Regex = struct
  let re_pattern str = Re.compile (Re.Perl.re str)

  let match_regex pattern str =
    try Some (Re.exec pattern str) with Not_found -> None

  let get_capture group match_result =
    try Some (Re.Group.get match_result group) with Not_found -> None

  let match_and_capture pattern str =
    match match_regex pattern str with
    | Some result -> (
        (* Get the first capture group *)
        match get_capture 1 result with
        | Some captured -> Some captured
        | None -> None)
    | None -> None
end

(* Example usage *)

type token = Semi_colon | String
type matcher = { pattern : Re.re; token : token option }
type token_value = { token : token; value : string }

let matchers =
  [
    { pattern = Regex.re_pattern "^([a-z]+)$"; token = Some String };
    { pattern = Regex.re_pattern "^;$"; token = Some Semi_colon };
  ]

type t = { mutable _string : string; mutable cursor : int }

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

let rec get_next_token tokenizer (token_matchers : matcher list) =
  if not (has_more_tokens tokenizer) then None
  else
    let remaining_string = get_remaining_string tokenizer in
    let rec loop_matchers t (token_matchers : matcher list) str =
      match token_matchers with
      | [] -> None
      | { pattern; token } :: _matchers -> (
          let extracted_value = match_token_pattern pattern str t in
          match (token, extracted_value) with
          | None, _ -> get_next_token t matchers
          | _, None -> loop_matchers t matchers str
          | Some token, Some value -> Some { token; value })
    in
    loop_matchers tokenizer token_matchers remaining_string

let get_next_token tokenizer = get_next_token tokenizer matchers
let make _string = { _string; cursor = 0 }

let init string tokenizer =
  tokenizer._string <- string;
  tokenizer.cursor <- 0
