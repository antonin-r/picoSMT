open Types

(* Theory structures *)

let print_th_exp exp =
  match exp with
  | Eq  (var1, var2) -> Printf.printf "%i=%i" var1 var2
  | Neq (var1, var2) -> Printf.printf "%i<>%i" var1 var2

let rec print_th_dij dij =
  match dij with 
  | []     -> print_newline ()
  | h :: t -> print_th_exp h ; print_char ' ' ; print_dij t

let rec print_th_cnf th_cnf =
  List.iter print_th_dij th_cnf

let print_th_cnfp th_cnfp =
  let nvar, ndij, cnf = th_cnfp in
  let () = print_endline "c This file is an output of Parser.print" in
  let () = Printf.printf "p %i %i\n" nvar ndij in
  print_th_cnf cnf

(* Sat structures *)

let print_sat_exp sat_exp =
  match sat_exp with
  | Y v -> print_int v
  | N v -> print_char '!'; print_int v

let print_sat_dij sat_dij =
  List.iter (fun x -> print_sat_exp h; print_char ' ') sat_dij;
  print_newline ();

let print_sat_cnf sat_cnf = 
  List.iter print_sat_dij sat_cnf

let rec print_sat_res res = 
  let b, _, _ = List.hd res in
  print_int (Sat_assoc.cardinal b);
  print_string "\n";
  print_string "-----\n";
  List.iter
    (function (x, y) ->
        print_int x;
        print_string " -> ";
        print_int (if fst y then 1 else 0);
        print_string " ; ";
        print_int (snd y);
        print_string "\n";
    ) (Sat_assoc.bindings b);;
