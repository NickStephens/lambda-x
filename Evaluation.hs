module Evaluation where
import LamTranslator
import Encode
import Lambda
import LambdaCore
import Sub


lamcbv = (App y lamcbvrec)
lamcbvrec = (Lam "f" (Lam "term" (App (App evallamguard (Var "term")) (App (App pr (App (App pr evalmatchvar) (Var "term"))) (App (App pr (App (App pr evalmatchlam) (Var "term"))) (App (App pr (App (App pr evalmatchapp) (App (App cbvcase (Var "f")) (Var "term")))) nil))))))




cbvcase = (Lam "f" (Lam "term" (App (App (App evalmatchlam (App (Var "f") (App extractaone (Var "term")))) (App (Var "f") (App (App (App lamsub (App (Var "f") (App extractatwo (Var "term")))) (App (App pr three) (App extractlname (App (Var "f") (App extractaone (Var "term")))))) (App extractlexp (App (Var "f") (App extractaone (Var "term"))))))) (App (Var "f") (App (App pr two) (App (App pr (App (Var "f") (App extractaone (Var "term")))) (App (Var "f") (App extractatwo (Var "term")))))))))
testcasevar = (Var "x")
testcaselam = (Lam "a" (App (Var "b") (Var "c")))
testcaseapp = (App (App plus one) one)

evalmatchvar = (Lam "arg" (App (App eq three) (App frst (Var "arg"))))
evalmatchlam = (Lam "arg" (App (App eq one) (App frst (Var "arg"))))
evalmatchapp = (Lam "arg" (App (App eq two) (App frst (Var "arg"))))

evallamguard = (App y (Lam "f" (Lam "i" (Lam "ps" (App (App (App (App frst (App car (Var "ps"))) (Var "i")) (App scnd (App car (Var "ps")))) (App (App (Var "f") (Var "i")) (App cdr (Var "ps"))))))))
fakef = (Lam "arg" (Var "arg"))
