type var = int
type exp = Eq of var * var | Neq of var * var
type dij = exp list
type cnf = int * int * dij list
