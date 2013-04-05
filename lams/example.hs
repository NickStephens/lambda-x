import Lambda
zero = (Lam "f" (Lam "x" (Var "x")))
one = (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
two = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x")))))
