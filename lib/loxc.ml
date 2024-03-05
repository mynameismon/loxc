let run program =
  Lex.scan_tokens program |> Lex.print_tree
