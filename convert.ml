open Types

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
    let () = Hashtbl.add table pair !boolvar in
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
    | Eq (var1, var2)  -> Y (Hashtbl.find table (var1, var2))
    | Neq (var1, var2) -> N (Hashtbl.find table (var1, var2))
  in
  let exp_of_literal literal =
    match literal with
    | Y i -> let (x, y) = arr.(i) in Eq (x, y)
    | N i -> let (x, y) = arr.(i) in Neq (x, y)
  in
  literal_of_exp, exp_of_literal

let cnf f =
  let g x = List.map (List.map f) x in
  g
