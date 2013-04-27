-- Abstract Syntax for PCONS


data Def = Def Name Func
		deriving (Show, Eq, Ord)

data Func = Cl Args Expr | Rec Args Expr
		deriving (Show, Eq, Ord)

type Args = [Expr]

data Expr = App Expr Expr | Lam Name Expr | Var Name | Let Name Expr Expr |
			Case Expr [(Expr, Expr)] | Cond Expr Expr Expr | UnOp Oper Expr |
			BinOp Oper Expr Expr | Val Value | Lst [Expr]
				deriving (Show, Eq, Ord)

type Name = String

data Oper = Add | Sub | Mul | Div | Not |
		deriving (Show, Eq, Ord)

data Value = F Float | I Int | C Char | B Bool
		deriving (Show, Eq, Ord)





