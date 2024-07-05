let run program =
  (* Lex.scan_tokens program |> Parse.parse |> Interpret.interpret *)
  Lex.scan_tokens program |> Parse.parse |> Ast.print_ast
