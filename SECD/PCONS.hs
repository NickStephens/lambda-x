-- Abstract Syntax for PCONS
module PCONS where

import SEC

type Script = [Def]

data Def = Def Name AFunc
		deriving (Show, Eq, Ord)

data AFunc = Cl Args Expr | Rec Args Expr | TRec Args Expr
		deriving (Show, Eq, Ord)

type Args = [Expr]

data Expr = App Expr Expr | Lam Expr | Var Name | Let Name Expr Expr |
			Case Expr [(Expr, Expr)] | Cond Expr Expr Expr | UnOp Oper Expr |
			BinOp Oper Expr Expr | Val AVal | Lst [Expr]
				deriving (Show, Eq, Ord)

type Name = String

--data Oper = Add | Sub | Mul | Div | Mod | Not | Neg
--		deriving (Show, Eq, Ord)

data AVal = AF Float | AI Int | AC Char | AB Bool
		deriving (Show, Eq, Ord)



fac = App (App (Var "fac") (App (App (Lam (Lam (BinOp Mul (Var "a") (Var "n")))) (Var "x")) (Var "y"))) 
		(App (Lam (BinOp Sub (Var "a") (Val$AI 1))) (Var "x"))

ts1 = App (Lam (App (Lam (BinOp Add (BinOp Mul (Var "n") (Var "m")) (Var "m"))) (Val$AI 2))) (Val$AI 3)

ts3 = App (Lam (BinOp Mul (Var "x") (Val$AI 2)))
	(App (Lam (BinOp Add (Var "x") (Val$AI 2)))
	(App (Lam (App (Lam (BinOp Mul (Var "n") (Var "m"))) (Val$AI 2))) (Val$AI 3)))



ts4 = Cond (App (Lam (BinOp Lt (Var "x") (Val$AI 3))) (Val$AI 2)) ts1 ts3


ts5 = Let "t" (Val$AI 2) (Cond (App (Lam (BinOp Lt (Var "x") (Val$AI 3))) (Var "t")) ts1 ts3)

