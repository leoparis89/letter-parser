(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

open Letter_parser
open Parser

let parse_number () =
  let result = Parser.make () |> Parser.parse " 42;   " in
  let expected = Program [ Expression_Statement (Literal (Numeric 42)) ] in
  assert (result = expected)

let parse_string () =
  let result = Parser.make () |> Parser.parse "\n\n  \"hello\"; " in
  let expected = Program [ Expression_Statement (Literal (String "hello")) ] in
  assert (result = expected)

let ignore_single_line_comment () =
  let result =
    Parser.make ()
    |> Parser.parse "//one comment       \n  \"hello\";   //other comment"
  in
  let expected = Program [ Expression_Statement (Literal (String "hello")) ] in
  assert (result = expected)

let ignore_multi_line_comment () =
  let result =
    Parser.make ()
    |> Parser.parse
         {|
      /* one comment 7 
      *  hey 99 "hello there" 
      */
      88;  //cool
    |}
  in
  let expected = Program [ Expression_Statement (Literal (Numeric 88)) ] in
  assert (result = expected)

let multiple_statements () =
  let result =
    Parser.make ()
    |> Parser.parse
         {|
      /* one comment 7 
      *  hey 99 "hello there" 
      */
      88;  //cool
      "hello";
    |}
  in
  let expected = Program [ Expression_Statement (Literal (Numeric 88)) ] in
  assert (result = expected)

let unexpected_token () =
  try
    let _ = Parser.make () |> Parser.parse "foo" in
    Alcotest.fail "Expected Parser.parse to fail with Failure"
  with Failure msg ->
    Alcotest.(check string)
      "should fail with unexpected token" "Unexpected token: f" msg

(* Run it *)
let () =
  let open Alcotest in
  run "Utils"
    [
      ( "string-case",
        [
          test_case "Test parser on number" `Quick parse_number;
          test_case "Test parser on string" `Quick parse_string;
          test_case "Test parser on unexpected token" `Quick unexpected_token;
          test_case "Test parser on comment" `Quick ignore_single_line_comment;
          test_case "Test parser on multi-line comment" `Quick
            ignore_multi_line_comment;
        ] );
    ]
