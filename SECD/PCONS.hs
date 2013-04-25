data Def = Def Name Func

data Func = Cl [Exp] Exp | Rec [Exp] Exp

data Exp = EA App | EL Lam | EV Var | ELT Let | EC Case |
			EM Numeric | ELS List | ECND IfThen | EB Boolean | EVL Value

data App = Ap Exp Exp

data Lam = Lm Var Exp

data Var = Vr Name

type Name = String

data Let = Let Name Exp Exp

data IfThen = Cond Exp Exp Exp

data Case = Cs Exp [(Exp, Exp)]

data Boolean = Bl BoolOp Value Value

data BoolOp = And | Or

data Numeric = Nu Oper Value Value

data Oper = Add | Sub | Mul | Div

data Value = I Int | C Char | B Bool

data List = Lst [Value]
