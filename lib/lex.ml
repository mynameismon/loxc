open Tokens

type context = {
    line: int;
    current: int;
    start: int;
    tokens: Tokens.token list;
  }

let char_to_string chars = chars |> List.rev |> List.to_seq |> String.of_seq

let add_token ?(current = 1) context token =
  {line = context.line;
   current = context.current + current;
   start = context.current;
   tokens = {kind = token;
             line_no = context.line;
             col = context.current} :: context.tokens}

let rec lex_comments chars =
  match chars with
  | [] -> chars
  | '\n' :: _ -> chars
  | _ :: t -> lex_comments t

(* Note: Multiline strings are not allowed because I am too lazy to figure out how to make  *)
let rec scan_string curr_str chars context =
  match chars with
  | [] -> [], {context with current = context.current + 1;
                        start = context.current;
                        tokens = {
                            kind = Error "String not closed!";
                            line_no = context.line;
                            col = context.current;} :: context.tokens}
  | '\n' :: t -> t, {line = context.line + 1;
                      current = 1;
                      start = 1;
                      tokens = {
                          kind = Error "String not closed!";
                          line_no = context.line;
                          col = context.current;} :: context.tokens}
  | '"' :: t -> t, {context with current = context.current + 1;
                        start = context.current;
                        tokens = {
                            kind = Ok (Tokens.String (char_to_string curr_str));
                            line_no = context.line;
                            col = context.current;} :: context.tokens}
  | c :: t -> scan_string (c :: curr_str) t {context with current = context.current + 1}

(* Idea: we have an implicit zipper like data structure. The current token that is being processed is held in curr.
   If the current token is recognised as a valid token, it is added to the parse tree and returned in processed *)
let rec scan_token remaining context =
  match remaining with
  | [] -> context
  | '\n' :: t -> scan_token t {context with line = context.line + 1; start = 1; current = 1}
  | ' ' :: t | '\t' :: t | '\r' :: t -> scan_token t
                                           {context with start = context.current;  current = context.current + 1}
  | '*' :: t -> scan_token t (add_token context (Ok Tokens.Star))
  | '+' :: t -> scan_token t (add_token context (Ok Tokens.Plus))
  | '-' :: t -> scan_token t (add_token context (Ok Tokens.Minus))
  | '{' :: t -> scan_token t (add_token context (Ok Tokens.LeftBrace))
  | '}' :: t -> scan_token t (add_token context (Ok Tokens.RightBrace))
  | '(' :: t -> scan_token t (add_token context (Ok Tokens.LeftParen))
  | ')' :: t -> scan_token t (add_token context (Ok Tokens.RightParen))
  | '!' :: '=' :: t -> scan_token t (add_token  ~current:2 context (Ok Tokens.BangEqual))
  | '!' :: t -> scan_token t (add_token context (Ok Tokens.Bang))
  | '=' :: '=' :: t -> scan_token t (add_token  ~current:2 context (Ok Tokens.EqualEqual))
  | '=' :: t -> scan_token t (add_token context (Ok Tokens.Equal))
  | '>' :: '=' :: t -> scan_token t (add_token  ~current:2 context (Ok Tokens.GreaterEqual))
  | '>' :: t -> scan_token t (add_token context (Ok Tokens.Greater))
  | '<' :: '=' :: t -> scan_token t (add_token  ~current:2 context (Ok Tokens.LessEqual))
  | '<' :: t -> scan_token t (add_token context (Ok Tokens.Less))
  | '/' :: '/' :: t -> scan_token (lex_comments t)
                         {context with line = context.line + 1; start = 1; current = 1}(* Handle comments gracefully *)
  | '/' :: t -> scan_token t (add_token context (Ok Tokens.Slash))
  | '"' :: t -> let result = (scan_string [] t context) in
                (match result with
                 | tokens, context -> scan_token tokens context)
  | ':' :: t -> scan_token t (add_token context (Ok Tokens.Colon)) (* Lox Extension: Lexing for type checking *)
  | '"' :: t -> let tokens, context = (scan_string [] t context) in scan_token tokens context
  | ':' :: t -> scan_token t (add_token context (Ok Tokens.Slash)) (* Lox Extension: Lexing for type checking *)
  | h :: t -> scan_token t (add_token context (Error (Printf.sprintf "Unknown token %c" h)))


let string_to_list s =
  s |> String.to_seq |> List.of_seq


let init_context = {line = 1;
                    current = 1;
                    start = 1;
                    tokens = []}
let scan_tokens program = (scan_token (string_to_list program) init_context).tokens

let rec print_tree_internal token_list output = 
  match token_list with
  | [] -> output
  | h :: t -> print_tree_internal t ((Tokens.print_token h) :: output)

let print_tree token_list =
  print_tree_internal token_list [] |> String.concat "\n"
