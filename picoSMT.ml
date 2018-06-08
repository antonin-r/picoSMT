open Types
open Functors

let filenames = ref []

let test = ref false
let test_list = 
  [
  ]

let verbose = ref false

let parse_anonymous str =
  if !test then
    if str = "all" then
      List.iter (fun x -> fst (snd (x)) := true) test_list
    else
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

let process_file filename =
  let th_cnfp = Parser.parse_file filename in
  let () = print_endline "Theory cnf" in
  let () = Printer.print_th_cnfp th_cnfp in
  let _, _, th_cnf = th_cnfp in
  let loe, eol = Convert.mk_converter th_cnfp in
  let sat_cnf = Convert.cnf loe th_cnf in
  let () = print_endline "Sat cnf" in
  let () = Printer.print_sat_cnf sat_cnf in
  let sat_res = Sat.dpll sat_cnf in
  let () = print_endline "Sat res" in
  let () = Printer.print_sat_res sat_res in
  let sat_assig =
    match sat_res with
    | []     -> assert false
    | (sat_assig, _, _) :: t -> sat_assig
  in
  let bindings = Sat_assoc.bindings sat_assig in
  let distrib = Array.of_list (List.map snd bindings) in
  let distrib = Array.map fst distrib in
  let () = print_endline "Distribution" in
  let () = 
    Array.iter (fun x -> if x then print_char 't' else print_char 'f') distrib;
    print_newline ()
  in
  ()

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
    let () = print_endline "Testing things" in
    List.iter process_file !filenames

;;
main ()
