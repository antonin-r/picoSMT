type var = int

type literal = Y of var | N of var
type ld = literal list
type cnf = ld list
type assig
val dpll : cnf -> assig list
val test : unit -> unit
