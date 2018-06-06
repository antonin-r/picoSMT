let filename = ref ""

let parse = ref false

let set_file_name name =
   let () = filename := name in
   ()

let main () = 
  let () = 
    Arg.parse [
    ("-parse", Arg.Set parse, "Stop after parsing the file")
    ]
    set_file_name
    "Take the name of the input file as argument"
  in
  if !parse then
    let th_cnf = Parser.parse_file !filename in
    ()
  else
  ()
