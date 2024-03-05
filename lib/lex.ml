type position = {
    line: int;
    current: int;
    start: int;
}

(* Idea: we have an implicit zipper like data structure. The current token that is being processed is held in curr.
   If the current token is recognised as a valid token, it is added to the parse tree and returned in processed *)
let rec scan_token remaining processed =
  match remaining with
  | [] -> processed
  | '\n' :: t -> scan_token t processed
  | '*' :: t -> scan_token t ((Ok Tokens.Star) :: processed)
  | '+' :: t -> scan_token t ((Ok Tokens.Plus) :: processed)
  | h :: t -> scan_token t ((Error (Printf.sprintf "Unknown token %c" h)) :: processed)


let string_to_list s =
  s |> String.to_seq |> List.of_seq

let scan_tokens program = scan_token (string_to_list program) [] |> List.rev

let print_tree token_list =
  let rec print_tree_internal token_list output = 
    match token_list with
    | [] -> output
    | h :: t ->
       match h with
       | Ok token -> print_tree_internal t (output ^ (Printf.sprintf "(%s)\n" (Tokens.token_to_string token)))
       | Error err_str -> print_tree_internal t (output ^ (Printf.sprintf "(Error: %s)\n" err_str)) in
  print_tree_internal token_list ""


