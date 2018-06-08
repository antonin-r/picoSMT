let rec iter_dij f dij =
  match dij with
  | []     -> ()
  | h :: t -> f h ; iter_dij f t

let rec iter_dijs f dijs =
  match dijs with
  | []     -> ()
  | h :: t -> iter_dij f h ; iter_dijs f t

let extract_pair exp =
  match exp with
  | Eq (var1, var2)  -> var1, var2
  | Neq (var1, var2) -> var1, var2

let add_hashtbl_exp table boolvar exp =
  let pair = extract_pair exp in
  if not (Hashtbl.mem table pair) then
    let () = Hashtbl.add pair !boolvar in
    incr boolvar

let add_array_exp table arr exp =
  let pair = extract_pair exp in
  arr.(Hashtbl.find table pair) <- pair

let mk_converter cnf =
  let (nvar, ndij, dijs) = cnf in
  let table = Hashtbl.create nvar in
  let boolvar = ref 0 in
  let f = add_hashtbl_exp table boolvar in
  let () = iter_dijs f dijs in
  let arr = Array.make !boolvar (0, 0) in
  let f = add_array_exp table arr in
  let () = iter_dijs f dijs in
  let literal_of_exp exp =
    match exp with
    | Eq (var1, var2)  -> Ast.Id (Hashtbl.find table (var1, var2))
    | Neq (var1, var2) -> Ast.Not (Hashtbl.find table (var1, var2))
  in
  let exp_of_literal literal =
    match literal with
    | Ast.Id i  -> Eq arr.(i)
    | Ast.Not i -> Neq arr.(i)
  in
  literal_of_exp, exp_of_literal

(* Testing *)

let rec aux_test filel =
  match filel with
  | []     -> ()
  | h :: t ->
      let cnf = parse_file ("test/" ^ h) in
      let literal_of_exp, exp_of_literal = mk_converter cnf in
      let 

let test () =
  let () = print_endline "Test : Convert : Started" in
  let filel = Array.to_list (Sys.readdir "test") in
  aux_test filel

