type term = int
type node = Var of Th_ast.var | Funct of Th_ast.funct * term list
type dag = node array
type constructing_dag = node list

type exp = Eq of term * term | Neq of term * term
type dij = exp list
type cnf = dij list

type term_hashtbl = (Th_ast.term, term) Hashtbl.t

let rec process_term dag ttbl new_node_ref term =
  if Hashtbl.mem ttbl term then
    Hashtbl.find ttbl term
  else
    let node = !new_node_ref in
    let () = incr new_node_ref in
    let () = Hashtbl.add ttbl term node in
    let () =
      match term with
      | Th_ast.Var v -> 
          dag := Var v :: !dag
      | Th_ast.Funct (f, argl) ->
          let nodel = List.map (process_term dag ttbl new_node_ref) argl in
          dag := Funct (f, nodel) :: !dag
    in
    node

let of_cnf cnf : (dag * cnf * term_hashtbl) =
  let _, nfunct, _, dijs = cnf in
  let ttbl = Hashtbl.create nfunct in
  let i = ref 0 in
  let dag = ref [] in
  let process_exp (l : dij) (exp : Th_ast.exp) : dij =
    let sub_process = process_term dag ttbl i in
    match exp with
    | Th_ast.Eq (term1, term2)  -> 
        (Eq (sub_process term1, sub_process term2)) :: l
    | Th_ast.Neq (term1, term2) ->
        (Neq (sub_process term1, sub_process term2)) :: l
  in
  let process_dij (l : cnf) (dij : Th_ast.dij) : cnf = 
    (List.fold_left process_exp [] dij) :: l
  in
  (Array.of_list !dag, List.fold_left process_dij [] dijs, ttbl)

(* Testing *)

let rec aux_test filel =
  match filel with
  | []     -> ()
  | h :: t ->
      let thcnf = Parser.parse_file ("test/" ^ h) in
      let (dag, cnf, ttbl) = of_cnf thcnf in
      Printf.printf "Test : Parsing : %s : NOT FAILED\n" h ;
      aux_test t

let test () =
  let () = print_endline "Test : Dag : Started" in
  let filel = Array.to_list (Sys.readdir "test") in
  aux_test filel

