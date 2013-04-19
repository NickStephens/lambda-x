module Evaluation where
import Lambda
import LambdaCore
import LamTranslator
casetest = (Lam "term" (App (App casE (Var "term")) patterns))
patterns = (App (App pr (App (App pr one) three)) (App (App pr (App (App pr two) two)) (App (App pr (App (App pr three) one)) nil)))




