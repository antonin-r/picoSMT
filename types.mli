open Functors

type var = int

type sat_exp = Y of var | N of var
type sat_dij = sat_exp list
type sat_cnf = sat_dij list

type sat_ext_literal = bool * int
type sat_assig = sat_ext_literal Sat_assoc.t
type sat_ext_assig = sat_assig * int * int
type sat_res = sat_ext_assig list

type th_exp = Eq of var * var | Neq of var * var
type th_dij = th_exp list
type th_cnf = th_dij list
type th_cnfp = int * int * th_cnf
type th_puf = Puf.t

type th_res = Thtrue | Thfalse of th_exp list
