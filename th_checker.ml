let rec separate (eq, neq) expl =
  match l with
  | []            -> eq, neq
  | Eq pair :: t  -> separate (pair :: eq, neq) t
  | Neq pair :: t -> separate (eq, pair :: neq) t

let rec check_neq s neq =
  match neq with
  | []            -> true
  | (v1, v2) :: t -> (Uf.find s v1 = Uf.find s v2) && check_neq s t

let check nvar expl =
  let eq, neq = separate ([], []) expl in
  let s = Uf.create nvar in
  let f = fun (v1, v2) -> Uf.union s v1 v2 in
  let () = List.iter f eq in
  check_neq s neq
