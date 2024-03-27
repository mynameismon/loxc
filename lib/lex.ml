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


(* Idea: Match as many numbers as one can. Once that is done, check if
   a period (.) is encountered. If yes, try to match as many numbers
   after that as possible. If we do not have a digit after that, we concede
   defeat and raise an error message. Else we have successful floating
   point number!*)
let scan_num chars context =
  let rec scan_int curr_str chars context =
    match chars with
    | '0'..'9' as num :: t -> scan_int (num :: curr_str) t {context with current = context.current + 1;}
    | _  ->  curr_str, chars, context in
  let pre, rem, context = scan_int [] chars context in
  let post, rem, context = match rem with
    | '.' :: '0'..'9':: t -> scan_int ('.' :: pre) t {context with current = context.current + 1;}
    | _ -> pre, rem, context in
  let token = match post with
    | '.' :: _ -> Error "Number ends with .!"
    | _ -> (Tokens.Number (Float.of_string (char_to_string post))) in
  let token_len = context.current - context.start in
  rem, (add_token ~current:token_len context token)

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
                            kind = (Tokens.String (char_to_string curr_str));
                            line_no = context.line;
                            col = context.current;} :: context.tokens}
  | c :: t -> scan_string (c :: curr_str) t {context with current = context.current + 1}

let scan_identifier chars context =
  let rec scan_word curr_str chars context = 
  match chars with
  | 'A'..'Z' | 'a'..'z' | '_' | '0'..'9' as h :: t -> scan_word (h :: curr_str) t {context with current = context.current + 1}
  | _ :: _ -> (char_to_string curr_str), chars, context
  | [] -> (char_to_string curr_str), chars, context in
  let identifier, chars, context = scan_word [] chars context in
  chars, (add_token context (match_keyword identifier))

(* Idea: we have an implicit zipper like data structure. The current token that is being processed is held in curr.
   If the current token is recognised as a valid token, it is added to the parse tree and returned in processed *)
let rec scan_token remaining context =
  match remaining with
  | [] -> context
  | '\n' :: t -> scan_token t {context with line = context.line + 1; start = 1; current = 1}
  | ' ' :: t | '\t' :: t | '\r' :: t -> scan_token t
                                           {context with start = context.current;  current = context.current + 1}
  | '*' :: t -> scan_token t (add_token context Tokens.Star)
  | '+' :: t -> scan_token t (add_token context Tokens.Plus)
  | '-' :: t -> scan_token t (add_token context Tokens.Minus)
  | '{' :: t -> scan_token t (add_token context Tokens.LeftBrace)
  | '}' :: t -> scan_token t (add_token context Tokens.RightBrace)
  | '(' :: t -> scan_token t (add_token context Tokens.LeftParen)
  | ')' :: t -> scan_token t (add_token context Tokens.RightParen)
  | '!' :: '=' :: t -> scan_token t (add_token  ~current:2 context Tokens.BangEqual)
  | '!' :: t -> scan_token t (add_token context Tokens.Bang)
  | '=' :: '=' :: t -> scan_token t (add_token  ~current:2 context Tokens.EqualEqual)
  | '=' :: t -> scan_token t (add_token context Tokens.Equal)
  | '>' :: '=' :: t -> scan_token t (add_token  ~current:2 context Tokens.GreaterEqual)
  | '>' :: t -> scan_token t (add_token context Tokens.Greater)
  | '<' :: '=' :: t -> scan_token t (add_token  ~current:2 context Tokens.LessEqual)
  | '<' :: t -> scan_token t (add_token context Tokens.Less)
  | '/' :: '/' :: t -> scan_token (lex_comments t)
                         {context with line = context.line + 1; start = 1; current = 1}(* Handle comments gracefully *)
  | '/' :: t -> scan_token t (add_token context Tokens.Slash)
  | '"' :: t -> let tokens, context = (scan_string [] t context) in scan_token tokens context
  | '0'..'9' :: _ -> let tokens, context = (scan_num remaining context) in scan_token tokens context
  | 'a'..'z' :: _ | 'A'..'Z' :: _ | '_' :: _ -> let tokens, context = (scan_identifier remaining context) in scan_token tokens context
  | ':' :: t -> scan_token t (add_token context Tokens.Colon) (* Lox Extension: Lexing for type checking *)
  | h :: t -> scan_token t (add_token context (Error (Printf.sprintf "Unknown token %c" h)))


let string_to_list s =
  s |> String.to_seq |> List.of_seq

let init_context = {line = 1;
                    current = 1;
                    start = 1;
                    tokens = []}
let scan_tokens program = List.rev ((scan_token (string_to_list program) init_context).tokens)

let rec print_tree_internal token_list output = 
  match token_list with
  | [] -> output
  | h :: t -> print_tree_internal t ((Tokens.print_token h) :: output)

let print_tree token_list =
  print_tree_internal (List.rev token_list) [] |> String.concat "\n"
