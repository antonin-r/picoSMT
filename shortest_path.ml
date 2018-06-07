type graph = int list array

(* Bread first search algorithm *)

type node_status = Visited of int | Unknown

let print_status stat =
  match stat with
  | Visited i -> print_int i
  | Unknown -> print_char 'u'

exception Found

let process_adj parent e status new_to_process adj =
  match status.(adj) with
  | Visited _ -> new_to_process
  | Unknown    -> 
      let () = status.(adj) <- Visited parent in
      adj :: new_to_process

let rec process_adjs parent e status new_to_process adjl =
  match adjl with
  | []     -> new_to_process
  | h :: t -> 
      let new_new_to_process = process_adj parent e status new_to_process h in
      if h = e then 
        raise Found
      else
        process_adjs parent e status new_new_to_process t

let process_node graph e status new_to_process node =
  let () = 
    match status.(node) with
    | Unknown -> assert false
    | _      -> ()
  in
  process_adjs node e status new_to_process graph.(node)

let rec step graph e status to_process new_to_process =
  match to_process with
  | []     -> new_to_process
  | h :: t ->
      try 
        let new_new_to_process = 
          process_node graph e status new_to_process h 
        in
        step graph e status t new_new_to_process
      with Found -> []

let bfs graph i e =
  let status = Array.make (Array.length graph) Unknown in
  let () = status.(i) <- Visited i in
  let to_process = ref [i] in
  let () =
    while (!to_process <> []) do
      to_process := step graph e status !to_process []
    done
  in
  status

(* BFS testing *)

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

(* Shortest path retreival *)

let rec retreive_path status i node l =
  if node = i then
    i :: l
  else
    match status.(node) with
    | Unknown        -> assert false
    | Visited parent -> retreive_path status i parent (node :: l)

let shortest_path graph i e =
  let status = bfs graph i e in
  retreive_path status i e []
      
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
