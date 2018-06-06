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
      let () = assert (String.length nvar = 1) in
      let () = assert (String.length ndij = 1) in
      (int_of_char nvar.[0], int_of_char ndij.[0])
  | _ -> assert false

let parse_exp nvar cond =
  let () = assert ((String.length cond = 3 || String.length cond = 4)) in
  if cond.[1] = '=' then
    let () = assert (String.length cond = 3) in
    let var1 = int_of_char cond.[0] in
    let var2 = int_of_char cond.[2] in
    let () = assert (var1 < nvar) in
    let () = assert (var2 < nvar) in
    Eq (var1, var2)
  else
  if cond.[1] = '<' then
    let () = assert (String.length cond = 4) in
    let () = assert (cond.[2] = '>') in
    let var1 = int_of_char cond.[0] in
    let var2 = int_of_char cond.[3] in
    let () = assert (var1 < nvar) in
    let () = assert (var2 < nvar) in
    Neq (var1, var2)
  else
    assert false

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

