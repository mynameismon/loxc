type error =
  | LexError of string
  | SynError of string
  | RunTimeError of string
  | InternalError of string

let print_error error =
  match error with
  | LexError str -> Printf.sprintf "Lex Error: %s" str
  | SynError str -> Printf.sprintf "Syntax Error: %s" str
  | RunTimeError str -> Printf.sprintf "Runtime Error: %s" str
  | InternalError str -> Printf.sprintf "INTERNAL ERROR!: %s" str
