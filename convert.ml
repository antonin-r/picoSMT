open Types
open Functors

let rec iter_dij f dij =
  match dij with
  | []     -> ()
  | h :: t -> f h ; iter_dij f t

let rec iter_dijs f dijs =
  match dijs with
  | []     -> ()
  | h :: t -> iter_dij f h ; iter_dijs f t

let normalize_pair (v1, v2) =
  if v1 < v2 then
    (v1, v2)
  else
  if v2 < v1 then
    (v2, v1)
  else
    assert false

let extract_pair exp =
  match exp with
  | Eq (var1, var2)  -> normalize_pair (var1, var2)
  | Neq (var1, var2) -> normalize_pair (var1, var2)

let add_hashtbl_exp table boolvar exp =
  let pair = extract_pair exp in
  if not (Hashtbl.mem table pair) then
    let () = Hashtbl.add table pair !boolvar in
    incr boolvar

let add_array_exp table arr exp =
  let pair = extract_pair exp in
  arr.(Hashtbl.find table pair) <- normalize_pair pair

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
    | Eq (var1, var2)  -> Y (Hashtbl.find table (normalize_pair (var1, var2)))
    | Neq (var1, var2) -> N (Hashtbl.find table (normalize_pair (var1, var2)))
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

let retreive_sat_expl sat_res =
  let sat_assig =
    match sat_res with
    | []     -> assert false
    | (sat_assig, _, _) :: t -> sat_assig
  in
  let bindings = Sat_assoc.bindings sat_assig in
  let build_sat_exp (i, (b, unused)) =
    if b then Y i else N i
  in
  List.map build_sat_exp bindings

let retreive_sat_order sat_res =
  List.map (fun (x,y,z) -> z) sat_res

let invert th_exp =
  match th_exp with
  | Eq (var1, var2)  -> Neq (var1, var2)
  | Neq (var1, var2) -> Eq (var1, var2)
