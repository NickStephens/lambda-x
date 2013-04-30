-- Abstract Syntax for PCONS
module PCONS where

import SEC (Oper (Add, Sub, Mul, Div, Mod, Not, Neg, Lt, Gt, Equ))

data Expr = 

		App Expr Expr | Lam Expr | Var Name | Val AVal |

		Let Name Expr Expr | Case Expr [(Expr, Expr)] | Cond Expr Expr Expr |

		UnOp Oper Expr | BinOp Oper Expr Expr |

		CLst [Expr] | Nil | Cdr Expr | Car Expr | Cons Expr Expr |

		Def Name Params Expr | RDef Name Params Expr | Clo Expr | Rec Name | 

		TRM Expr | TRCL Expr Expr Expr | RCL Expr Expr Expr | CNT Expr | TNT Expr

				deriving (Show, Eq, Ord)

type Name = String
type Params = [Name]

data AVal = AF Float | AI Int | AC Char | AB Bool
		deriving (Show, Eq, Ord)

ts1 = App (Lam (App (Lam (BinOp Add (BinOp Mul (Var "n") (Var "m")) (Var "m"))) (Val$AI 2))) (Val$AI 3)

ts2 = App (Lam (BinOp Add (Val$AI 2) (Var "x"))) (Val$AI 3)

ts3 = App (Lam (BinOp Mul (Var "x") (Val$AI 2)))
	(App (Lam (BinOp Add (Var "x") (Val$AI 2)))
	(App (Lam (App (Lam (BinOp Mul (Var "n") (Var "m"))) (Val$AI 2))) (Val$AI 3)))

ts4 = Cond (BinOp Lt (Val$AI 4) (Val$AI 3)) ts1 ts3

ts5 = Let "t" (Val$AI 2) (Cond (BinOp Lt (Var "t") (Val$AI 3))  ts1 ts3)

ts6 = App (Lam (BinOp Sub (Var "n") (Val$AI 2))) (App (Lam (Car (Var "x"))) ls)

ls = Cons (Val$AI 2) Nil

em = App (Lam (App (Lam (Cons (Var "x") (Var "y"))) (Val$AI 3))) Nil

ts8 = App (Lam (App (Lam (App func (Val$AI 5))) (Val$AI 4))) (Val$AI 3)

func = Def "func" ["x", "y", "z"] (Lam (BinOp Sub (Var "x") (BinOp Sub (Var "y") (Var "z"))))

pfunc = RDef "pf" ["c", "a"]
	(RCL (BinOp Equ (Var "c") (Val$AI 1)) 
	(TRM (Var "a"))
	(CNT (Cons ( BinOp Sub (Var "c") (Val$AI 1) ) (Cons (BinOp Mul (Var "c") (Var "a")) Nil))) )

fract = App (Lam (App pfunc (Val$AI 6))) (Val$AI 1)


tfunc = RDef "pf" ["c", "a"]
	(RCL (BinOp Equ (Var "c") (Val$AI 1)) 
	(TRM (Var "a"))
	(TNT (Cons ( BinOp Sub (Var "c") (Val$AI 1) ) (Cons (BinOp Mul (Var "c") (Var "a")) Nil))) )

trac = App (Lam (App tfunc (Val$AI 6))) (Val$AI 1)




