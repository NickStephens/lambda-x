module Sub where
import Lambda
import LambdaCore
import LamTranslator
import Encode
lamsub = (App y lamsubrec)



term = (App (App pr two) (App (App pr (App (App pr three) one)) (App (App pr three) two)))


var = (App (App pr three) one)


m = (App (App pr three) four)

testsub = (App (App (App lamsub m) var) term)
lamsubrec = (Lam "f" (Lam "m" (Lam "var" (Lam "term" (App (App lamguard (App (App pr (Var "term")) (App (App pr (Var "var")) (App (App pr (Var "m")) (App (App pr (Var "f")) nil))))) (App (App pr (App (App pr matchvar) varaction)) (App (App pr (App (App pr matchapp) appaction)) (App (App pr (App (App pr matchlam) lamaction)) nil))))))))















extractv = scnd

extractlname = (Lam "lam" (App frst (App scnd (Var "lam"))))

extractlexp = (Lam "lam" (App scnd (App scnd (Var "lam"))))

extractaone = (Lam "app" (App frst (App scnd (Var "app"))))

extractatwo = (Lam "app" (App scnd (App scnd (Var "app"))))



matchvar = (Lam "terms" (App (App eq three) (App car (Var "terms"))))
matchlam = (Lam "terms" (App (App eq one) (App car (Var "terms"))))
matchapp = (Lam "terms" (App (App eq two) (App car (Var "terms"))))






varaction = (Lam "list" (App (App (App (App eq (App extractv (App car (App cdr (Var "list"))))) (App extractv (App car (Var "list")))) (App car (App cdr (App cdr (Var "list"))))) (App car (Var "list"))))




lamaction = (Lam "list" (App (App (App (App eq (App extractv (App car (App cdr (Var "list"))))) (App extractlname (App car (Var "list")))) (App car (Var "list"))) (App (App pr one) (App (App pr (App extractlname (App car (Var "list")))) (App (App (App (App car (App cdr (App cdr (App cdr (Var "list"))))) (App car (App cdr (App cdr (Var "list"))))) (App car (App cdr (Var "list")))) (App extractlexp (App car (Var "list"))))))))


appaction = (Lam "list" (App (App pr two) (App (App pr (App (App (App (App car (App cdr (App cdr (App cdr (Var "list"))))) (App car (App cdr (App cdr (Var "list"))))) (App car (App cdr (Var "list")))) (App extractaone (App car (Var "list"))))) (App (App (App (App car (App cdr (App cdr (App cdr (Var "list"))))) (App car (App cdr (App cdr (Var "list"))))) (App car (App cdr (Var "list")))) (App extractatwo (App car (Var "list")))))))

vartestlist = (App (App pr (App (App pr three) two)) (App (App pr (App (App pr three) one)) (App (App pr (Var "m")) (App (App pr (Var "f")) nil))))
lamtestlist = (App (App pr (App (App pr one) (App (App pr three) (App (App pr three) zero)))) (App (App pr (App (App pr three) four)) (App (App pr (Var "m")) (App (App pr (Var "f")) nil))))
apptestlist = (App (App pr (App (App pr two) (App (App pr one) two))) (App (App pr (App (App pr three) four)) (App (App pr (Var "m")) (App (App pr (Var "f")) nil))))



lamguard = (App y (Lam "f" (Lam "i" (Lam "ps" (App (App (App (App frst (App car (Var "ps"))) (Var "i")) (App scnd (App car (Var "ps")))) (App (App (Var "f") (Var "i")) (App cdr (Var "ps"))))))))
