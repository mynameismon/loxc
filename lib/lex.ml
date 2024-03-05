open Tokens

type 'a result = Ok of 'a | Error of string

type context = {
    line: int;
    current: int;
    start: int;
    tokens: Tokens.token list;
  }

let add_token ?(current = 1) context token =
  {line = context.line;
   current = context.current + current;
   start = context.current;
   tokens = {kind = token;
             line_no = context.line;
             col = context.current} :: context.tokens}

(* Idea: we have an implicit zipper like data structure. The current token that is being processed is held in curr.
   If the current token is recognised as a valid token, it is added to the parse tree and returned in processed *)
let rec scan_token remaining context =
  match remaining with
  | [] -> context
  | '*' :: t -> scan_token t (add_token context (Ok Tokens.Star))
  | '+' :: t -> scan_token t (add_token context (Ok Tokens.Plus))
  | '-' :: t -> scan_token t (add_token context (Ok Tokens.Minus))
  | '{' :: t -> scan_token t (add_token context (Ok Tokens.LeftBrace))
  | '}' :: t -> scan_token t (add_token context (Ok Tokens.RightBrace))
  | '(' :: t -> scan_token t (add_token context (Ok Tokens.LeftParen))
  | ')' :: t -> scan_token t (add_token context (Ok Tokens.RightParen))
  | h :: t -> scan_token t (add_token context (Error (Printf.sprintf "Unknown token %c" h)))


let string_to_list s =
  s |> String.to_seq |> List.of_seq


let init_context = {line = 1;
                    current = 0;
                    start = 0;
                    tokens = []}
let scan_tokens program = (scan_token (string_to_list program) init_context).tokens |> List.rev

let rec print_tree_internal token_list output = 
  match token_list with
  | [] -> output
  | h :: t -> print_tree_internal t ((Tokens.print_token h) :: output)

let print_tree token_list =
  print_tree_internal token_list [] |> String.concat "\n"
