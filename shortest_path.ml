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
