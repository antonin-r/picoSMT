open Types
open Functors

(* Theory structures *)

let print_th_exp exp =
  match exp with
  | Eq  (var1, var2) -> Printf.printf "%i=%i" var1 var2
  | Neq (var1, var2) -> Printf.printf "%i<>%i" var1 var2

let rec print_th_dij dij =
  match dij with 
  | []     -> print_newline ()
  | h :: t -> print_th_exp h ; print_char ' ' ; print_th_dij t

let rec print_th_cnf th_cnf =
  List.iter print_th_dij th_cnf

let print_th_cnfp th_cnfp =
  let nvar, ndij, cnf = th_cnfp in
  let () = print_endline "c This file is an output of Parser.print" in
  let () = Printf.printf "p %i %i\n" nvar ndij in
  print_th_cnf cnf

let print_th_res th_res =
  match th_res with
  | Thtrue              -> print_endline "Match !"
  | Thfalse explanation ->
      print_endline "Impossible";
      print_th_dij explanation

(* Sat structures *)

let print_sat_exp sat_exp =
  match sat_exp with
  | Y v -> print_int v
  | N v -> print_char '!'; print_int v

let print_sat_dij sat_dij =
  List.iter (fun x -> print_sat_exp x; print_char ' ') sat_dij;
  print_newline ()

let print_sat_cnf sat_cnf = 
  List.iter print_sat_dij sat_cnf

let rec print_sat_assoc assoc =
  Printf.printf "Nb defined lits : %i\n" (Sat_assoc.cardinal assoc);
  print_string "-----\n";
  List.iter
    (function (x, y) ->
        Printf.printf "%i -> %i   (%i)\n" x (if fst y then 1 else 0) (snd y)
    ) 
    (Sat_assoc.bindings assoc)

let rec print_sat_res res = 
  let last_assoc, _, _ = List.hd res in
  print_sat_assoc last_assoc
