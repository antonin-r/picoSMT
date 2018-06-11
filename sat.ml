open Types
open Functors

exception Not_sat

let extract_id = function
  | Y v -> v
  | N v -> v

let extract_value = function
  | Y v -> true
  | N v -> false

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

let debug id (assig : Types.sat_assig) nb_decisions id_last_decision =
  Printf.printf ("Id :               %i\nNb decision :      %i\n" ^^ 
    "Id last decision : %i\n")
    id nb_decisions id_last_decision;
  List.iter 
    (fun (i, (b, d)) -> Printf.printf "%i:%i(%i) " i (if b then 1 else 0) d) 
    (Sat_assoc.bindings assig);
  print_newline ();
  print_newline ()

(*
- first step : check whether the formula contains an unsatisfiable clause,
taken independantly of the others
- second step : check whether the formula contains any clause of length <= 1 ;
such clauses determine the value of a possible solution and can be removed,
and the concerned variables set to the corresponding values
- third step (?) : check whether there are variables that are present in the
formula always under the same literal form ; such variables can be fixed,
###
it is important to note that the assignments made in this pre-treatment (2 and
 3) are not handled as "classical" decisions
*)
let check_formula_1 f =
  let rec check_clause_1 acc = function
      [] -> true
    | l::c ->
      let literal_id = extract_id l in
      let literal_value = extract_value l in
      if Sat_assoc.mem literal_id acc
      then
        if literal_value = Sat_assoc.find literal_id acc
        then
          check_clause_1 acc c
        else
          false
      else
        check_clause_1 (Sat_assoc.add literal_id literal_value acc) c
  in
  List.for_all (function c -> check_clause_1 (Sat_assoc.empty) c) f

let rec check_formula_2 acc acc_f = function
    [] -> true, acc, acc_f
  | c::cs ->
    if List.length c = 0
    then
      check_formula_2 acc acc_f cs
    else
      if List.length c = 1
      then
        let literal = List.hd c in
        let literal_id = extract_id literal in
        let literal_value = extract_value literal in
        if Sat_assoc.mem literal_id acc
        then
          if literal_value = fst (Sat_assoc.find literal_id acc)
          then
            check_formula_2 acc acc_f cs
          else
            false, acc, acc_f
        else
          check_formula_2 (Sat_assoc.add literal_id (literal_value, -1) acc) acc_f cs
      else
        check_formula_2 acc (c::acc_f) cs

let check_formula f =
  if check_formula_1 f
  then
    let b, preassig, new_f = check_formula_2 (Sat_assoc.empty) [] f in
    if b
    then
      new_f, preassig
    else
      raise Not_sat
  else
    raise Not_sat

let rec aux_clause f id assigs acc = function
    [] -> id+1, assigs
  | l::clause ->
    let assig, nb_decisions, id_last_decision = List.hd assigs in
    
    debug id assig nb_decisions id_last_decision;

    let literal_id = extract_id l in

    if satisfy f assig
    then
      begin
      (* SAT *)
      print_string ">>>SAT\n";
      -2, assigs
      end
    else
      begin
        if satisfy_c (acc@[l]@clause) assig
        then
          begin
          (* next *)
          print_string ">>>Next clause\n";
          id+1, assigs
          end
        else
          begin
            if acc <> [] && not (satisfy_c acc assig) && not (Sat_assoc.mem literal_id assig)
            then
              (* unit *)
              begin
                print_string ">>>Unit\n";
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
                    print_string ">>>Decide\n";
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
                      print_string ">>>Continue\n";
                      aux_clause f id assigs (l::acc) clause
                    end
                  else
                    begin
                      if not (satisfy_c (l::acc) assig) && nb_decisions <> 0
                      then
                        (* backtrack *)
                        begin
                          print_string ">>>Backtrack\n";
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
                          print_string ">>>Fail\n";
                          raise Not_sat
                        end
                    end
              end
          end
      end

(* let assigs = [(Sat_assoc.empty, 0, -1)] in *)
let dpll f assigs =
  (* let decisions = Sat_assoc.singleton (-1) true in *)
  let clauses = Array.of_list f in
  let lgth_clauses = Array.length clauses in

  let b = ref (0, assigs) in
  while (fst !b) >= 0 && (fst !b) < lgth_clauses do
    Printf.printf "Clause : %i \n" (fst !b);
    b := aux_clause f (fst !b) (snd !b) [] clauses.(fst !b);
  done;
  snd !b
