(* Is it possible to have Bracket = LeftParen | RightParen,
   Operator = Plus | Minus | Star, etc.
   and then make TokenType = Bracket | Operator | ...?*)

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

type token = {
    kind: (token_type, string) result;
    line_number: int;
  }

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
  | Identifier idf -> idf;
