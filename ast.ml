type var = int

module Assc = Map.Make(var)

type literal = L_id of var | L_not of var
(* DPLL : decision literal *)
type ext_literal = True | False | Decision

(* disjunction of literals *)
type ld = literal list

(* conjunctive normal form *)
type cnf = ld list

(* assignment *)
(* type assig = ext_literal list * int *)
type assig = ext_literal Assc.t


let extract_id = function
	| L_id v -> v
	| L_not v -> v

let convert = function
	| True -> true
	| False -> false
	| Decision -> failwith "at this point, all decisions should have been made"


let satisfy_c c m = 
  List.exists (function l -> convert (Assc.find (extract_id l) m)) c

let satisfy f m = List.for_all (function c -> satisfy_c c m) f

let dpll f =


