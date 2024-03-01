let read_whole_file _filename =
  failwith "function not implemented"

let read_from_stdin () =
  failwith "function not implemented"

let repl () =
  failwith "function not implemented"

let main () =
  match Sys.argv with
  | [| _; "run"; filename |] -> filename |> read_whole_file |> Loxc.run
  | [| _; "repl" |] -> repl
  | [| _; "help" |] | _ ->
     Printf.printf
"loxc [run filename | repl | help]\n\
 \n\
 run\t  Reads and runs file\n\
 repl\t Opens an interative replace\n\
 help\t Prints this message\n";;

let () =
  main()
