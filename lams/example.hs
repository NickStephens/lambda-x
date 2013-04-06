module Example where
import Lambda

zero = (Lam "f" (Lam "x" (Var "x")))
one = (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
true = (Lam "x" (Lam "y" (Var "x")))
false = (Lam "x" (Lam "y" (Var "y")))
lif = (Lam "bool" (Lam "then" (Lam "else" (App (App (Var "bool") (Var "then")) (Var "else")))))

test = (Lam "f" (App (App (App (App (Var "f") lif) true) one) zero))
