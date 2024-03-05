let read_whole_file _filename =
  failwith "function not implemented";;

let rec repl () =
  print_string "loxc> ";
  try
    let input = read_line () in
    let result = eval input in 
    print_endline result;
    repl ()  (* Loop back to read the next input *)
  with
  | End_of_file -> exit 65;
  | exn -> 
     print_endline ("Error: " ^ Printexc.to_string exn); exit 65
and eval input = Loxc.run input |> Printf.sprintf "%s" ;;

let main () =
  match Sys.argv with
  | [| _; "run"; filename |] -> filename |> read_whole_file |> Loxc.run |> Printf.printf "%s"
  | [| _; "repl" |] -> repl()
  | [| _; "help" |] | _ ->  Printf.printf
 "loxc [run filename | repl | help]\n\
 \n\
 run\t  Reads and runs file\n\
 repl\t Opens an interative replace\n\
 help\t Prints this message\n";;

let () =
  main()
