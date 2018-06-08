open Th_ast

(* Parser *)

let arity_arr = ref [||]

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
  | magic :: cnf :: nvar :: nfun :: ndij :: [] ->
      let () = assert (magic = "p") in
      let () = assert (cnf = "cnf") in
      (int_of_string nvar, int_of_string nfun, int_of_string ndij)
  | _ -> assert false

let split_args args =
  let level = ref 0 in
  let start = ref 0 in
  let index = ref 0 in
  let len = String.length args in
  let res = ref [] in
  let () =
    while !index < len do
      let () =
        if (args.[!index] = ',') && !level = 0 then
          let () = assert (!start < !index) in
          let () = res := (String.sub args !start (!index - !start)) :: !res in
          start := !index + 1 
        else
        if (args.[!index] = '(') then
          incr level
        else
        if (args.[!index] = ')') then
          let () = assert (!level > 0) in
          decr level
        else
          ()
      in
      incr index
    done
  in
  List.rev ((String.sub args !start (String.length args - !start)) :: !res)

let rec parse_args nvar nfun args =
  let terml = split_args args in
  List.map (parse_term nvar nfun) terml

and parse_term nvar nfun term =
  if String.contains term '(' then 
    let () = assert (term.[String.length term - 1] = ')') in
    let i = String.index term '(' in
    let funct = String.sub term 0 i in
    let args = String.sub term (i + 1) (String.length term - (i + 2)) in
    let funct = int_of_string funct in
    let () = assert (funct < nfun) in
    let args = parse_args nvar nfun args in
    let arity = (!arity_arr).(funct) in
    let () =
      if arity = 0 then
        (!arity_arr).(funct) <- (List.length args)
      else
        assert (arity = List.length args)
    in
    Funct (funct, args)
  else
    let var = (int_of_string term) in
    let () = assert (var < nvar) in
    Var var

let parse_exp nvar nfun cond =
  if String.contains cond '=' then
    let i = String.index cond '=' in
    let term1 = String.sub cond 0 i in
    let term2 = String.sub cond (i + 1) (String.length cond - (i + 1)) in
    let term1 = parse_term nvar nfun term1 in
    let term2 = parse_term nvar nfun term2 in
    Eq (term1, term2)
  else
    let () = assert (String.contains cond '<') in
    let i = String.index cond '<' in
    let () = assert (String.length cond > i + 1) in
    let () = assert (cond.[i + 1] = '>') in
    let term1 = String.sub cond  0 i in
    let term2 = String.sub cond (i + 2) (String.length cond - (i + 2)) in
    let term1 = parse_term nvar nfun term1 in
    let term2 = parse_term nvar nfun term2 in
    Neq (term1, term2)

let rec parse_dij nvar nfun strl : dij =
  match strl with
  | []     -> []
  | h :: t -> (parse_exp nvar nfun h) :: (parse_dij nvar nfun t)

let rec parse_inc inc =
  try
    let line = input_line inc in
    line :: parse_inc inc
  with End_of_file -> []

let rec skip_com linel =
  match linel with
  | []     -> assert false
  | h :: t -> if is_com h then skip_com t else linel

let rec parse_dijs nvar nfun ndij linel : dij list =
  match linel with
  | []     -> 
      let () = assert (ndij = 0) in
      []
  | h :: t -> 
      (parse_dij nvar nfun (line_split h)) :: 
        (parse_dijs nvar nfun (ndij - 1) t)

let parse_file file_name : cnf =
  let inc = open_in file_name in
  let linel = parse_inc inc in
  let () = close_in inc in
  let linel = skip_com linel in
  match linel with
  | []     -> assert false
  | h :: t ->
      let () = assert (is_decl h) in
      let nvar, nfun, ndij = parse_decl (line_split h) in
      let () = arity_arr := Array.make nfun 0 in
      (nvar, ndij, nfun, parse_dijs nvar nfun ndij t)

(* Printer *)

let rec string_of_args argl =
  match argl with
  | []     -> assert false
  | [h]    -> string_of_term h
  | h :: t -> (string_of_term h) ^ "," ^ (string_of_args t)

and string_of_term term =
  match term with
  | Var v -> string_of_int v
  | Funct (f, argl) -> (string_of_int f) ^ "(" ^ (string_of_args argl) ^ ")"

let print_exp exp =
  match exp with
  | Eq  (term1, term2) -> 
      Printf.printf "%s=%s" (string_of_term term1) (string_of_term term2)
  | Neq (term1, term2) -> 
      Printf.printf "%s<>%s" (string_of_term term1) (string_of_term term2)

let rec print_dij dij =
  match dij with 
  | []     -> print_newline ()
  | h :: t -> print_exp h ; print_char ' ' ; print_dij t

let rec print_dijs dijl =
  match dijl with
  | []     -> ()
  | h :: t -> print_dij h ; print_dijs t

let print_cnf cnf =
  let nvar, ndij, nfunct, dijl = cnf in
  let () = print_endline "c This file is an output of Parser.print" in
  let () = Printf.printf "p %i %i %i\n" nvar nfunct ndij in
  print_dijs dijl

(* Test *)

let rec aux_test filel =
  match filel with
  | []     -> ()
  | h :: t -> 
      print_cnf (parse_file ("test/" ^ h)) ;
      Printf.printf "Test : Parsing : %s : PASSED\n" h ;
      aux_test t

let test () =
  let () = print_endline "Test : Parse : Started" in
  let filel = Array.to_list (Sys.readdir "test") in
  aux_test filel
