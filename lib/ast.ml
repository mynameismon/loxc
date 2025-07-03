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

type stmt =
  | Expr of expr
  | Print of expr
  | Var of string * expr
  | Error of error

type ast = stmt list

let rec print_expr token =
  match token with
  | Literal l -> (
      match l with
      | Nil -> "Nil"
      | Bool b -> Printf.sprintf "%b" b
      | Number n -> Printf.sprintf "%f" n
      | String str | Identifier str -> Printf.sprintf "\"%s\"" str)
  | Grouping g -> Printf.sprintf "(Grouping %s)" (print_expr g)
  | Unary (op, right) ->
      Printf.sprintf "(%s %s)" (Tokens.token_to_string op) (print_expr right)
  | Binary (left, op, right) ->
      Printf.sprintf "(%s %s %s)"
        (Tokens.token_to_string op)
        (print_expr left) (print_expr right)
  | Error str -> print_error str

let print_ast_node node =
  match node with
  | Expr expr -> Printf.sprintf "(Expr %s)" (print_expr expr)
  | Print expr -> Printf.sprintf "(Print %s)" (print_expr expr)
  | Var (name, expr) -> Printf.sprintf "(Var %s = %s)" name (print_expr expr)
  | Error err -> print_error err

let print_ast ast =
  List.map print_ast_node ast |> String.concat "\n" |> String.cat "Ast:\n"

(* Literal tests *)
let%test "print nil" = String.equal "Nil" (print_expr (Literal Nil))
let%test "print true" = String.equal "true" (print_expr (Literal (Bool true)))

let%test "print false" =
  String.equal "false" (print_expr (Literal (Bool false)))

let%test "print number" =
  String.equal "1.000000" (print_expr (Literal (Number 1.)))

let%test "print unary operator" =
  String.equal "(- 5.000000)"
    (print_expr (Unary (Tokens.Minus, Literal (Number 5.))))

let%test "print grouping" =
  String.equal "(- (Grouping false))"
    (print_expr (Unary (Tokens.Minus, Grouping (Literal (Bool false)))))

let%test "print binary operator" =
  String.equal "(+ 1.000000 2.000000)"
    (print_expr
       (Binary (Literal (Number 1.), Tokens.Plus, Literal (Number 2.))))
