type funct = int
type var = int
type term = Var of var | Funct of funct * term list
type exp = Eq of term * term | Neq of term * term
type dij = exp list
type cnf = int * int * int * dij list
