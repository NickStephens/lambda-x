module LambdaCore where
import Lambda
import LamParsec

tr = (Lam "a" (Lam "b" (Var "a")))
fl = (Lam "a" (Lam "b" (Var "b")))
iF = (Lam "p" (Lam "a" (Lam "b" (App (App (Var "p") (Var "a")) (Var "b")))))

empty = fl
isEmpty = (Lam "l" (App (App (Var "l") (Lam "a" (Lam "b" (Var "fl")))) tr))
cons = (Lam "h" (Lam "t" (Lam "c" (Lam "n" (App (App (Var "c") (Var "h")) (App (App (Var "t") (Var "c")) (Var "n")))))))
hd = (Lam "l" (App (App (Var "l") tr) fl))
tl = (Lam "l" (App frst (App (App (Var "l") (Lam "a" (Lam "b" (App (App pr (App scnd (Var "b"))) (App (App cons (Var "a")) (App scnd (Var "b"))))))) (App (App pr empty) empty))))

pr = (Lam "x" (Lam "y" (Lam "p" (App (App (Var "p") (Var "x")) (Var "y")))))
frst = (Lam "p" (App (Var "p") tr))
scnd = (Lam "p" (App (Var "p") fl))
oR = (Lam "p" (Lam "q" (App (App (Var "p") (Var "p")) (Var "q"))))
anD = (Lam "p" (Lam "q" (App (App (Var "p") (Var "q")) (Var "p"))))

y = (Lam "g" (App (Lam "x" (App (Var "g") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "g") (App (Var "x") (Var "x"))))))

zero = (Lam "f" (Lam "x" (Var "x")))
one = (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
two = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x")))))
three = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))
four = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))))

sucs = (Lam "n" (Lam "f" (Lam "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))))))
prdc = (Lam "n" (Lam "f" (Lam "x" (App (App (App (Var "n") (Lam "g" (Lam "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Lam "u" (Var "x"))) (Lam "u" (Var "u"))))))
isZr = (Lam "n" (App (App (Var "n") (Lam "x" fl)) tr))
subt = (Lam "m" (Lam "n" (App (App (Var "n") prdc) (Var "m"))))
