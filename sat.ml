open Types
open Functors

let extract_id = function
  | Y v -> v
  | N v -> v

let satisfy_c c m =
  List.exists (function l ->
    if Sat_assoc.mem (extract_id l) m
    then
      begin
        let b = fst (Sat_assoc.find (extract_id l) m) in match l with
          Y v -> b
        | N v -> not b
      end
    else false)
    c

let satisfy f m = List.for_all (function c -> satisfy_c c m) f

let debug id assig nb_decisions id_last_decision (* decisions *) =
  print_string "<\n<";
  print_int id;
  print_string "> ";
  print_int nb_decisions;
  print_string " ";
  print_int id_last_decision;
  print_string "\n";  
  print_string "assig:\n";
  List.iter
    (function (x, y) ->
        print_int x;
        print_string " -> ";
        print_int (if fst y then 1 else 0);
        print_string "\n";
    ) (Sat_assoc.bindings assig);
  print_string ">\n"


let rec aux_clause f id assigs acc = function
    [] -> id+1, assigs
  | l::clause ->
    let assig, nb_decisions, id_last_decision = List.hd assigs in
    
    print_string "\n";
    debug id assig nb_decisions id_last_decision;

    let literal_id = extract_id l in

    if satisfy f assig
    then
      begin
      (* SAT *)
      print_string "SAT\n";
      -2, assigs
      end
    else
      begin
        if satisfy_c (acc@[l]@clause) assig
        then
          begin
          (* next *)
          print_string "next clause\n";
          id+1, assigs
          end
        else
          begin
            if acc <> [] && not (satisfy_c acc assig) && not (Sat_assoc.mem literal_id assig)
            then
              (* unit *)
              begin
                print_string "unit\n";
                let converted_l = match l with
                  Y v -> true
                | N v -> false
                in
                let new_assig = Sat_assoc.add literal_id (converted_l, id) assig in
                let new_assigs = (new_assig, nb_decisions, id_last_decision) :: (List.tl assigs) in
                id+1, new_assigs
              end
            else
              begin
                if not @@ Sat_assoc.mem literal_id assig
                then
                  (* decide *)
                  begin
                    print_string "decide\n";
                    let converted_l = true
                    in
                    let new_assig = Sat_assoc.add literal_id (converted_l, id) assig in
                    let assig_1 = (new_assig, nb_decisions + 1, literal_id)
                    in
                    let assig_2 = (assig, nb_decisions, id_last_decision) in
                    let new_assigs = assig_1::assig_2::(List.tl assigs) in
                    aux_clause f id new_assigs (l::acc) clause
                  end
                else
                  if not (satisfy_c (l::acc) assig) && clause <> []
                  then
                    (* allright, why not, continue in this clause *)
                    begin
                      print_string "continue\n";
                      aux_clause f id assigs (l::acc) clause
                    end
                  else
                    begin
                      if not (satisfy_c (l::acc) assig) && nb_decisions <> 0
                      then
                        (* backtrack *)
                        begin
                          print_string "backtrack\n";
                          let backtrack_assig, _, old_id_last_decision = List.hd (List.tl assigs) in
                          let literal_value, literal_clause_def = Sat_assoc.find id_last_decision assig in

                          (* if old_id_last_decision = -1, then you can empty the assig *)
                          let limit_l_clause_def =
                            if Sat_assoc.mem id_last_decision assig
                            then
                              snd (Sat_assoc.find id_last_decision assig)
                            else
                              -1
                          in
                          let new_backtrack_assig = List.fold_left
                            (
                              function a ->
                              function b ->
                              let temp_l_clause_def = snd @@ Sat_assoc.find b assig
                              in
                              if temp_l_clause_def >= limit_l_clause_def
                              then Sat_assoc.remove b a
                              else a
                            )
                            assig
                            (List.map fst (Sat_assoc.bindings assig))
                          in

                          let new_backtrack_assig = Sat_assoc.add id_last_decision (not literal_value, literal_clause_def) new_backtrack_assig in

                          let new_backtrack_assig_tuple = (new_backtrack_assig, nb_decisions - 1, old_id_last_decision) in

                          let new_backtrack_assigs = new_backtrack_assig_tuple :: (List.tl (List.tl assigs)) in

                          literal_clause_def, new_backtrack_assigs
                        end
                      else
                        begin
                          print_string "fail\n";
                          -1, assigs
                        end
                    end
              end
          end
      end


let f1 = function
  | a, b -> a

let f2 = function
  | a, b -> b

let f3 = function
  | a, b, c -> c


let dpll f =
  let assigs = [(Sat_assoc.empty, 0, -1)] in
  let decisions = Sat_assoc.singleton (-1) true in
  let clauses = Array.of_list f in
  let lgth_clauses = Array.length clauses in

  let b = ref (0, assigs) in
  while (f1 !b) >= 0 && (f1 !b) < lgth_clauses do
    print_int (f1 !b); print_string "\n";
    b := aux_clause f (f1 !b) (f2 !b) [] clauses.(f1 !b);
  done;
  f2 !b
