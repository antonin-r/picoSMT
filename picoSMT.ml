let filenames = ref []

let test = ref false
let test_list = 
  [("parse", (ref false, Parser.test)); 
   ("bfs", (ref false, Fordfulk.testbfs)); 
   ("ff", (ref false, Fordfulk.test));
  ]

let verbose = ref false

let parse_anonymous str =
  if !test then
    if not (List.mem_assoc str test_list) then
      Printf.printf "Error : Unkown test '%s'\n" str
    else
      fst (List.assoc str test_list) := true
  else
    filenames := str :: !filenames

let rec run_tests testl =
  match testl with
  | []     -> print_endline "Tests done"
  | h :: t -> 
      (
        (if !(fst (snd h)) then
          (snd (snd h)) ()
        ) ;
        run_tests t
      )

let main () = 

  Arg.parse [
  ("-test", Arg.Set test, "Run tests on some part of the program");
  ("-verbose", Arg.Set verbose, 
      "Output many details about steps of resolution")
  ]
  parse_anonymous
  "Take the name of the input file as argument" ;

  if !test then
    run_tests test_list
  else
    print_endline "Solver not implemented yet"

;;
main ()
