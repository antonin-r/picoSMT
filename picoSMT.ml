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
  let () = print_newline () in

  let nvar, _, th_cnf = th_cnfp in
  let loe, eol = Convert.mk_converter th_cnfp in

  let sat_cnf = Convert.cnf loe th_cnf in
  let () = print_endline "Sat cnf" in
  let () = Printer.print_sat_cnf sat_cnf in
  let () = print_newline () in

  let reduced_sat_cnf, preassig = Sat.check_formula sat_cnf in
  let () = print_endline "Reduced sat cnf" in
  let () = Printer.print_sat_cnf reduced_sat_cnf in
  let () = print_newline () in
  let () = print_endline "Preassig" in
  let () = Printer.print_sat_assoc preassig in
  let () = print_newline () in

  let th_check_res = ref (Thfalse []) in
  let sat_cnf = ref reduced_sat_cnf in
  let sat_res = ref [(preassig, 0, -1)] in

  let () = 
    try

      while !th_check_res <> Thtrue do

      let () = sat_res := Sat.dpll !sat_cnf !sat_res in
      let () = print_endline "Sat res" in
      let () = Printer.print_sat_res !sat_res in
      let () = print_newline () in

      let sat_expl = Convert.retreive_sat_expl !sat_res in
      let () = print_endline "Sat expression list" in
      let () = Printer.print_sat_dij sat_expl in
      let () = print_newline () in

      let th_expl = List.map eol sat_expl in
      let () = print_endline "Th expression list" in
      let () = Printer.print_th_dij th_expl in
      let () = print_newline () in

      let new_th_check_res = Th_checker.check nvar th_expl in
      let () = th_check_res := new_th_check_res in

      match new_th_check_res with
      | Thtrue          -> print_endline "Match!"
      | Thfalse th_expl -> 
          let () = print_endline "Impossible !" in
          let () = print_endline "Explanation :" in
          let () = Printer.print_th_dij th_expl in
          let () = print_newline () in
          let f x = loe (Convert.invert x) in
          let () = sat_cnf := ((List.map f th_expl) :: !sat_cnf) in
          Printer.print_sat_cnf !sat_cnf
      done;

    with Sat.Not_sat ->
      print_endline "Not SAT !"
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
