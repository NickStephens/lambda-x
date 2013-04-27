-- Abstract Syntax for PCONS
module PCONS where

import SEC

type Script = [Def]

data Def = Def Name AFunc
		deriving (Show, Eq, Ord)

data AFunc = Cl Args Expr | Rec Args Expr | TRec Args Expr
		deriving (Show, Eq, Ord)

type Args = [Expr]

data Expr = App Expr Expr | Lam Name Expr | Var Name | Let Name Expr Expr |
			Case Expr [(Expr, Expr)] | Cond Expr Expr Expr | UnOp Oper Expr |
			BinOp Oper Expr Expr | Val AVal | Lst [Expr]
				deriving (Show, Eq, Ord)

type Name = String

--data Oper = Add | Sub | Mul | Div | Mod | Not | Neg
--		deriving (Show, Eq, Ord)

data AVal = AF Float | AI Int | AC Char | AB Bool
		deriving (Show, Eq, Ord)



fac = App (App (Var "fac") (App (App (Lam "a" (Lam "n" (BinOp Mul (Var "a") (Var "n")))) (Var "x")) (Var "y"))) 
		(App (Lam "a" (BinOp Sub (Var "a") (Val$AI 1))) (Var "x"))

