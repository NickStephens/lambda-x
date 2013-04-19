module Sub where
import Lambda
import LambdaCore
import LamTranslator
import Encode















extract = scnd

matchvar = (Lam "terms" (App (App eq three) (App car (Var "terms"))))
matchlam = (Lam "terms" (App (App eq one) (App car (Var "terms"))))
matchapp = (Lam "terms" (App (App eq two) (App car (Var "terms"))))

varaction = (Lam "list" (App (App (App (App eq (App extract (App car (App cdr (Var "list"))))) (App extract (App car (Var "list")))) (App car (App cdr (App cdr (Var "list"))))) (App car (Var "list"))))


vartestlist = (App (App pr (App (App pr three) two)) (App (App pr (App (App pr three) one)) (App (App pr (Var "m")) (App (App pr (Var "f")) nil))))










lamgaurd = (App y (Lam "f" (Lam "i" (Lam "ps" (App (App (App (App frst (App car (Var "ps"))) (Var "i")) (App scnd (App car (Var "ps")))) (App (App (Var "f") (Var "i")) (App cdr (Var "ps"))))))))
