val mk_converter : Th_ast.cnf -> 
  (Th_ast.exp -> Sat.literal) * (Sat.literal -> Th_ast.exp)

val cnf : ('a -> 'b) -> 'a list list -> 'b list list 
