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

let empty_block () =
  let result = Parser.make () |> Parser.parse "{}" in
  let expected = Program [ Block_Statement [] ] in
  assert (result = expected)

let block_with_statement () =
  let result = Parser.make () |> Parser.parse "{1;}" in
  let expected =
    Program [ Block_Statement [ Expression_Statement (Literal (Numeric 1)) ] ]
  in
  assert (result = expected)

let block_with_multiple_statements () =
  let result =
    Parser.make ()
    |> Parser.parse
         {|
      /* test block with multiple statements
      *  
      */
      {1;
       // comment
      "foo";
      // comment
      3;}
    |}
  in
  let expected =
    Program
      [
        Block_Statement
          [
            Expression_Statement (Literal (Numeric 1));
            Expression_Statement (Literal (String "foo"));
            Expression_Statement (Literal (Numeric 3));
          ];
      ]
  in
  assert (result = expected)

let block_with_nested_block () =
  let result =
    Parser.make ()
    |> Parser.parse
         {|
      {1;4;  {8; 9;}
      // comment
      "foo";}
    |}
  in
  let expected =
    Program
      [
        Block_Statement
          [
            Expression_Statement (Literal (Numeric 1));
            (* Block_Statement
              [
                Expression_Statement (Literal (Numeric 2));
                Expression_Statement (Literal (String "foo"));
              ]; *)
            Expression_Statement (Literal (Numeric 4));
            Block_Statement
              [
                Expression_Statement (Literal (Numeric 8));
                Expression_Statement (Literal (Numeric 9));
              ];
            Expression_Statement (Literal (String "foo"));
          ];
      ]
  in
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
  run "Letter parser"
    [
      ( "string-case",
        [
          test_case "Test parser on number" `Quick parse_number;
          test_case "Test parser on string" `Quick parse_string;
          test_case "Test parser on unexpected token" `Quick unexpected_token;
          test_case "Test parser on comment" `Quick ignore_single_line_comment;
          test_case "Test parser on multi-line comment" `Quick
            ignore_multi_line_comment;
          test_case "Test parser on empty block" `Quick empty_block;
          test_case "Test parser on block with statement" `Quick
            block_with_statement;
          test_case "Test parser on block with multiple statements" `Quick
            block_with_multiple_statements;
          test_case "Test parser on block with nested block" `Quick
            block_with_nested_block;
        ] );
    ]
