val mk_converter : cnf -> 
  (Th_ast.exp -> Ast.literal) * (Ast.literal -> Th_ast.exp)
