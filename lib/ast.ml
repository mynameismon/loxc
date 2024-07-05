open Error

type literal =
  | Nil
  | Bool of bool
  | Identifier of string
  | Number of float
  | String of string

type expr =
  | Literal of literal
  | Unary of Tokens.token_type * expr
  | Binary of expr * Tokens.token_type * expr
  | Grouping of expr
  | Error of error


let rec print_ast_node token =
  match token with
  | Literal l -> (
     match l with
     | Nil -> "Nil"
     | Bool b -> Printf.sprintf "%b" b
     | Number n -> Printf.sprintf "%f" n
     | String str | Identifier str -> Printf.sprintf "\"%s\"" str)
  | Grouping g -> Printf.sprintf "(Grouping %s)" (print_ast_node g)
  | Unary (op, right) -> Printf.sprintf "(%s %s)" (Tokens.token_to_string op) (print_ast_node right)
  | Binary (left, op, right) -> Printf.sprintf "(%s %s %s)"  (Tokens.token_to_string op) (print_ast_node left) (print_ast_node right)
  | Error str -> print_error str

let print_ast tokens = print_ast_node tokens

(* Literal tests *)
let%test "print nil" = (String.equal "Nil" (print_ast_node (Literal(Nil))))
let%test "print true" = (String.equal "true" (print_ast_node (Literal(Bool true))))
let%test "print false" = (String.equal "false" (print_ast_node (Literal(Bool false))))
let%test "print number" = (String.equal "1.000000" (print_ast_node (Literal(Number(1.)))))

let%test "print unary operator" = String.equal "(- 5.000000)" (print_ast_node (Unary (Tokens.Minus,
                                                                                      Literal(Number 5.))))
let%test "print grouping" = String.equal "(- (Grouping false))" (print_ast_node (Unary (Tokens.Minus,
                                                                                        Grouping (Literal(Bool false)))))
let%test "print binary operator" = String.equal "(+ 1.000000 2.000000)" (print_ast_node (Binary
                                                                                           ((Literal(Number 1.)),
                                                                                            Tokens.Plus,
                                                                                            (Literal(Number 2.)))))
