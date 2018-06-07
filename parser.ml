open Th_ast

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

let rec parse_dij nvar strl : dij =
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

let rec parse_dijs nvar ndij linel : dij list =
  match linel with
  | []     -> 
      let () = assert (ndij = 0) in
      []
  | h :: t -> 
      (parse_dij nvar (line_split h)) :: (parse_dijs nvar (ndij - 1) t)

let parse_file file_name : cnf =
  let inc = open_in file_name in
  let linel = parse_inc inc in
  let linel = skip_com linel in
  match linel with
  | []     -> assert false
  | h :: t ->
      let () = assert (is_decl h) in
      let nvar, ndij = parse_decl (line_split h) in
      (nvar, ndij, parse_dijs nvar ndij t)

let print_exp exp =
  match exp with
  | Eq  (var1, var2) -> Printf.printf "%i=%i" var1 var2
  | Neq (var1, var2) -> Printf.printf "%i<>%i" var1 var2

let rec print_dij dij =
  match dij with 
  | []     -> print_newline ()
  | h :: t -> print_exp h ; print_char ' ' ; print_dij t

let rec print_dijs dijl =
  match dijl with
  | []     -> ()
  | h :: t -> print_dij h ; print_dijs t

let print_cnf cnf =
  let nvar, ndij, dijl = cnf in
  let () = print_endline "c This file is an output of Parser.print" in
  let () = Printf.printf "p %i %i\n" nvar ndij in
  print_dijs dijl
