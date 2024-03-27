type literal =
  | Nil
  | Bool of bool
  | Identifier of string
  | Number of float
  | String of string

type expr =
  | Literal of literal
  | Unary of Tokens.token * expr
  | Binary of expr * Tokens.token * expr
  | Grouping of expr
  | Error of string

let rec print_ast_node token =
  match token with
  | Literal l -> (
     match l with
     | Nil -> "Nil"
     | Bool b -> Printf.sprintf "%b" b
     | Number n -> Printf.sprintf "%f" n
     | String str | Identifier str -> str)
  | Grouping g -> Printf.sprintf "(Grouping %s)" (print_ast_node g)
  | Unary (op, right) -> Printf.sprintf "(%s %s)" (Tokens.print_token op) (print_ast_node right)
  | Binary (left, op, right) -> Printf.sprintf "(%s %s %s)" (print_ast_node left) (Tokens.print_token op) (print_ast_node right)
  | Error str -> Printf.sprintf "(Error %s)" str

let print_ast tokens =
  List.rev_map print_ast_node tokens
