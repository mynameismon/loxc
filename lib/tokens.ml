(* Is it possible to have Bracket = LeftParen | RightParen,
   Operator = Plus | Minus | Star, etc.
   and then make TokenType = Bracket | Operator | ...?*)

open Error

type token_type =
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Plus
  | Minus
  | Star
  | Slash
  | Semicolon
  | Colon
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | String of string
  | Number of float
  | Identifier of string
  | Error of error

type token = { kind : token_type; line_no : int; col : int }

let match_keyword identifier =
  match identifier with
  | "and" -> And
  | "class" -> Class
  | "else" -> Else
  | "false" -> False
  | "for" -> For
  | "fun" -> Fun
  | "if" -> If
  | "nil" -> Nil
  | "or" -> Or
  | "print" -> Print
  | "return" -> Return
  | "super" -> Super
  | "this" -> This
  | "true" -> True
  | "var" -> Var
  | "while" -> While
  | _ -> Identifier identifier

let token_name token =
  match token with
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | Comma -> "Comma"
  | Dot -> "Dot"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Star -> "Star"
  | Slash -> "Slash"
  | Semicolon -> "Semicolon"
  | Colon -> "Colon"
  | Bang -> "Bang"
  | BangEqual -> "BangEqual"
  | Equal -> "Equal"
  | EqualEqual -> "EqualEqual"
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | And -> "And"
  | Class -> "Class"
  | Else -> "Else"
  | False -> "False"
  | Fun -> "Fun"
  | For -> "For"
  | If -> "If"
  | Nil -> "Nil"
  | Or -> "Or"
  | Print -> "Print"
  | Return -> "Return"
  | Super -> "Super"
  | This -> "This"
  | True -> "True"
  | Var -> "Var"
  | While -> "While"
  | String _ -> "String"
  | Number _ -> "Number"
  | Identifier _ -> "Identifier"
  | Error _ -> "Error"

let token_to_string token =
  match token with
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | Comma -> ","
  | Dot -> "."
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Semicolon -> ";"
  | Colon -> ":"
  | Bang -> "!"
  | BangEqual -> "!="
  | Equal -> "="
  | EqualEqual -> "=="
  | Greater -> ">"
  | GreaterEqual -> ">="
  | Less -> "<"
  | LessEqual -> "<="
  | And -> "and"
  | Class -> "class"
  | Else -> "else"
  | False -> "false"
  | Fun -> "fun"
  | For -> "for"
  | If -> "if"
  | Nil -> "nil"
  | Or -> "or"
  | Print -> "print"
  | Return -> "return"
  | Super -> "super"
  | This -> "this"
  | True -> "true"
  | Var -> "var"
  | While -> "while"
  | String str -> Printf.sprintf "\"%s\"" str
  | Number num -> Float.to_string num
  | Identifier idf -> idf
  | Error err -> Printf.sprintf "%s" (print_error err)

let print_token tok =
  Printf.sprintf "(Line %d, Col %d: %s %s)" tok.line_no tok.col
    (token_name tok.kind) (token_to_string tok.kind)
