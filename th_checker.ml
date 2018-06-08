open Types

(* Checking part *)

let rec separate (eq, neq) expl =
  match expl with
  | []                       -> eq, neq
  | Eq (v1, v2) :: t  -> separate ((v1, v2) :: eq, neq) t
  | Neq (v1, v2) :: t -> separate (eq, (v1, v2) :: neq) t

type check_neq_res = Cn_true | Cn_false of (int * int)

let rec check_neq s neq =
  match neq with
  | []            -> Cn_true
  | (v1, v2) :: t ->
      if Uf.find s v1 = Uf.find s v2 then
        Cn_false (v1, v2)
      else
        check_neq s t

(* Explaining part *)

let build_graph nvar l =
  let graph = Array.make nvar [] in
  let rec aux l =
    match l with
    | []            -> ()
    | (v1, v2) :: t ->
        graph.(v1) <- v2 :: graph.(v1);
        graph.(v2) <- v1 :: graph.(v2);
        aux t
  in
  aux l; graph

let get_edge path =
  let rec aux i l =
    match l with
    | []     -> []
    | h :: t -> (i, h) :: aux h t
  in
  match path with
  | []     -> []
  | h :: t -> aux h t

let get_explaination nvar eq v1 v2 =
  let path = Shortest_path.shortest_path (build_graph nvar eq) v1 v2 in
  let explaining_eql = get_edge path in
  let l = List.map (fun (x, y) -> Eq (x, y)) explaining_eql in
  Neq (v1, v2) :: l

(* Main *)

let check nvar expl =
  let eq, neq = separate ([], []) expl in
  let s = Uf.create nvar in
  let f = fun (v1, v2) -> Uf.union s v1 v2 in
  let () = List.iter f eq in
  match check_neq s neq with
  | Cn_true           -> RTrue
  | Cn_false (v1, v2) -> RFalse (get_explaination nvar eq v1 v2)
