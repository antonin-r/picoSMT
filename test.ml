open Types

(* Th_checker *)

let print_res res =
  match res with
  | RTrue       -> print_endline "True"
  | RFalse expl -> print_string "False "; Parser.print_dij expl

let test () = 
  let open Types in
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

(* BFS *)

let testbfs () =
  let () = print_endline "Test : Bfs : Started" in
  let graph1 = [|[1]; [0]|] in
  let graph2 = [|[]; []|] in
  let graph3 = [|[1; 2]; [3; 0; 2]; [1; 0]; [4; 1]; [3]|] in
  let sol1 = [|Visited 0; Visited 0|] in
  let sol2 = [|Visited 0; Unknown|] in
  let sol3 = [|Visited 0; Visited 0; Visited 0; Visited 1; Visited 3|] in
  let () = print_endline "Test : Bfs : Running test 1" in
  let res1 = bfs graph1 0 1 in
  let () = print_endline "Test : Bfs : Running test 2" in
  let res2 = bfs graph2 0 1 in
  let () = print_endline "Test : Bfs : Running test 3" in
  let res3 = bfs graph3 0 4 in
  let pres = fun b -> if b then "PASSED" else "FAILED" in
  Printf.printf "Test : Bfs : Test1 : %s\n" (pres (res1 = sol1));
  Printf.printf "Test : Bfs : Test2 : %s\n" (pres (res2 = sol2));
  Printf.printf "Test : Bfs : Test3 : %s\n" (pres (res3 = sol3))

(* Ff testing *)

let test () =
  let () = print_endline "Test : Sp : Started" in
  let graph1 = [|[1]; [0]|] in
  let graph2 = [|[1; 2]; [3; 0; 2]; [1; 0]; [4; 1]; [3]|] in
  let sol1 = [0; 1] in
  let sol2 = [0; 1; 3; 4] in
  let () = print_endline "Test : Sp : Running test 1" in
  let res1 = shortest_path graph1 0 1 in
  let () = print_endline "Test : Sp : Running test 2" in
  let res2 = shortest_path graph2 0 4 in
  let pres = fun b -> if b then "PASSED" else "FAILED" in
  Printf.printf "Test : Sp : Test1 : %s\n" (pres (res1 = sol1));
  Printf.printf "Test : Sp : Test2 : %s\n" (pres (res2 = sol2));
