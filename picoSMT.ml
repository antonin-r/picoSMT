let filename = ref ""

let parse = ref false
let verbose = ref false

let set_file_name name =
   let () = filename := name in
   ()

let main () = 
  let () = 
    Arg.parse [
    ("-parse", Arg.Set parse, "Stop after parsing the file");
    ("-verbose", Arg.Set verbose, 
        "Output many details about steps of resolution")
    ]
    set_file_name
    "Take the name of the input file as argument"
  in
  if !parse then
    let th_cnf = Parser.parse_file !filename in
    let () = if !verbose then Parser.print_cnf th_cnf in
    ()
  else
  ()

;;
main ()
