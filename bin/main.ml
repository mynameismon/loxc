let read_whole_file _filename =
  failwith "function not implemented";;

let rec repl () =
  print_string "loxc> ";
  try
    let input = read_line () in
    print_endline input;
    repl ()  (* Loop back to read the next input *)
  with
  | End_of_file -> print_endline "Exiting REPL";
  | exn -> 
    print_endline ("Error: " ^ Printexc.to_string exn);;

let main () =
  match Sys.argv with
  | [| _; "run"; filename |] -> filename |> read_whole_file |> Loxc.run
  | [| _; "repl" |] -> repl()
  | [| _; "help" |] | _ ->  Printf.printf
 "loxc [run filename | repl | help]\n\
 \n\
 run\t  Reads and runs file\n\
 repl\t Opens an interative replace\n\
 help\t Prints this message\n";;

let () =
  main()
