type res = RTrue | RFalse of Th_ast.exp list

(* Checking part *)

let rec separate (eq, neq) expl =
  match expl with
  | []                       -> eq, neq
  | Th_ast.Eq (v1, v2) :: t  -> separate ((v1, v2) :: eq, neq) t
  | Th_ast.Neq (v1, v2) :: t -> separate (eq, (v1, v2) :: neq) t

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
  let l = List.map (fun (x, y) -> Th_ast.Eq (x, y)) explaining_eql in
  Th_ast.Neq (v1, v2) :: l

(* Main *)

let check nvar expl =
  let eq, neq = separate ([], []) expl in
  let s = Uf.create nvar in
  let f = fun (v1, v2) -> Uf.union s v1 v2 in
  let () = List.iter f eq in
  match check_neq s neq with
  | Cn_true           -> RTrue
  | Cn_false (v1, v2) -> RFalse (get_explaination nvar eq v1 v2)

(* Testing *)

let print_res res =
  match res with
  | RTrue       -> print_endline "True"
  | RFalse expl -> print_string "False "; Parser.print_dij expl

let test () = 
  let open Th_ast in
  let () = print_endline "Test : Thcheck : Started" in
  let expl1 = [Eq (0, 1); Eq (1, 2); Eq (2, 0); Eq (3, 4); Eq (5, 3)] in
  let expl2 = [Neq (0, 1); Neq (1, 2); Neq (3, 4)] in
  let expl3 = [Eq (0, 1); Neq (1, 0)] in
  let expl4 = [Eq (0, 1); Eq (1, 2); Neq (2, 0)] in
  let expl5 = [Eq (0, 1); Eq (1, 2); Neq (2, 3); Eq (3, 4)] in
  let () = print_endline "Test : Thcheck : Running test1" in
  let res1 = check 6 expl1 in
  let () = print_res res1 in
  let () = print_endline "Test : Thcheck : Running test2" in
  let res2 = check 5 expl2 in
  let () = print_res res2 in
  let () = print_endline "Test : Thcheck : Running test3" in
  let res3 = check 2 expl3 in
  let () = print_res res3 in
  let () = print_endline "Test : Thcheck : Running test4" in
  let res4 = check 3 expl4 in
  let () = print_res res4 in
  let () = print_endline "Test : Thcheck : Running test5" in
  let res5 = check 5 expl5 in
  let () = print_res res5 in
  let is_true res =
    match res with
    | RTrue -> true
    | _    -> false
  in
  let pres b = if b then "NOT FAILED" else "FAILED" in
  Printf.printf "Test : Thcheck : test1 : %s\n" (pres (is_true res1));
  Printf.printf "Test : Thcheck : test2 : %s\n" (pres (is_true res2));
  Printf.printf "Test : Thcheck : test3 : %s\n" (pres (not (is_true res3)));
  Printf.printf "Test : Thcheck : test4 : %s\n" (pres (not (is_true res4)));
  Printf.printf "Test : Thcheck : test5 : %s\n" (pres (is_true res5));
