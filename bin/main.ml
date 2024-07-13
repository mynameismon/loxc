let read_lines filename =
  let f = open_in filename in
  let rec loop () =
    try
      let next = input_line f in
      next :: loop ()
    with End_of_file ->
      close_in f;
      []
  in
  loop ()

let rec repl () =
  print_string "loxc> ";
  try
    let input = read_line () in
    let result = eval input in
    print_endline result;
    repl () (* Loop back to read the next input *)
  with
  | End_of_file -> exit 65
  | exn ->
      print_endline ("Error: " ^ Printexc.to_string exn);
      exit 65

and eval input = Loxc.run input

let main () =
  match Sys.argv with
  | [| _; "run"; filename |] ->
      read_lines filename |> String.concat "\n" |> Loxc.run |> print_endline
  | [| _; "repl" |] -> repl ()
  | [| _; "help" |] | _ ->
      Printf.printf
        "loxc [run filename | repl | help]\n\n\
         run\tReads and runs file\n\
         repl\tOpens an interative replace\n\
         help\tPrints this message\n"

let () = main ()
