import Lambda
zero = (Lam "f" (Lam "x" (Var "x")))
one = (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
true = (Lam "x" (Lam "y" (Var "x")))
false = (Lam "x" (Lam "y" (Var "y")))
if = (Lam "bool" (Lam "then" (Lam "else" (App (App (Var "bool") (Var "then")) (Var "else")))))
test = (App (App (App if true) one) zero)
