data Def = Def Name Func deriving (Show, Eq, Ord)

data Func = Cl [Expr] Expr | Rec [Expr] Expr deriving (Show, Eq, Ord)

data Expr = EA App | EL Lam | EV Var | ELT Let | EC Case |
			EM Numeric | ELS List | ECND IfThen | EB Boolean | EVL Value deriving (Show, Eq, Ord)

data App = Ap Expr Expr deriving (Show, Eq, Ord)

data Lam = Lm Var Expr deriving (Show, Eq, Ord)

data Var = Vr Name deriving (Show, Eq, Ord)

type Name = String

data Let = Let Name Expr Expr deriving (Show, Eq, Ord)

data IfThen = Cond Expr Expr Expr deriving (Show, Eq, Ord)

data Case = Cs Expr [(Expr, Expr)] deriving (Show, Eq, Ord)

data Boolean = Bl BoolOp Value Value deriving (Show, Eq, Ord)

data BoolOp = And | Or deriving (Show, Eq, Ord)

data Numeric = Nu Oper Value Value deriving (Show, Eq, Ord)

data Oper = Add | Sub | Mul | Div deriving (Show, Eq, Ord)

data Value = I Int | C Char | B Bool deriving (Show, Eq, Ord)

data List = Lst [Value] deriving (Show, Eq, Ord)
