let run program =
  Lex.scan_tokens program |> Parse.parse |> Interpret.interpret
