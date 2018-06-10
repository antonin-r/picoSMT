open Types
open Functors

(* Parser *)

let line_split =
  String.split_on_char ' '

let is_com str =
  let () = assert (String.length str > 0) in
  str.[0] = 'c'

let is_decl str =
  let () = assert (String.length str > 0) in
  str.[0] = 'p'

let parse_decl strl =
  match strl with
  | magic :: cnf :: nvar :: ndij :: [] ->
      let () = assert (magic = "p") in
      let () = assert (cnf = "cnf") in
      (int_of_string nvar, int_of_string ndij)
  | _ -> assert false

let parse_exp nvar cond =
  let () = print_endline cond in
  if String.contains cond '=' then
    let i = String.index cond '=' in
    let var1 = String.sub cond 0 i in
    let var2 = String.sub cond (i + 1) (String.length cond - (i + 1)) in
    let var1 = int_of_string var1 in
    let var2 = int_of_string var2 in
    let () = assert (var1 < nvar) in
    let () = assert (var2 < nvar) in
    Eq (var1, var2)
  else
    let () = assert (String.contains cond '<') in
    let i = String.index cond '<' in
    let () = assert (String.length cond > i + 1) in
    let () = assert (cond.[i + 1] = '>') in
    let var1 = String.sub cond  0 i in
    let var2 = String.sub cond (i + 2) (String.length cond - (i + 2)) in
    let var1 = int_of_string var1 in
    let var2 = int_of_string var2 in
    let () = assert (var1 < nvar) in
    let () = assert (var2 < nvar) in
    Neq (var1, var2)

let rec parse_dij nvar strl : th_dij =
  match strl with
  | []     -> []
  | h :: t -> (parse_exp nvar h) :: (parse_dij nvar t)

let rec parse_inc inc =
  try
    let line = input_line inc in
    line :: parse_inc inc
  with End_of_file -> []

let rec skip_com linel =
  match linel with
  | []     -> assert false
  | h :: t -> if is_com h then skip_com t else linel

let rec parse_dijs nvar ndij linel : th_cnf =
  match linel with
  | []     -> 
      let () = assert (ndij = 0) in
      []
  | h :: t -> 
      (parse_dij nvar (line_split h)) :: (parse_dijs nvar (ndij - 1) t)

let parse_file file_name : th_cnfp =
  let inc = open_in file_name in
  let linel = parse_inc inc in
  let () = close_in inc in
  let linel = skip_com linel in
  match linel with
  | []     -> assert false
  | h :: t ->
      let () = assert (is_decl h) in
      let nvar, ndij = parse_decl (line_split h) in
      (nvar, ndij, parse_dijs nvar ndij t)
