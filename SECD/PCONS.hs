-- Abstract Syntax for PCONS
module PCONS where

import SEC (Oper (Add, Sub, Mul, Div, Mod, Not, Neg, Lt, Gt, Equ, And, Or))

data Expression = 

		Apply Expression Expression | Lambda Expression | Variable Name | Value AVal |

		Let Name Expression Expression | Case Expression [(Expression, Expression)] | Cond Expression Expression Expression |

		UnOp Oper Expression | BinOp Oper Expression Expression |

		CLst [Expression] | Nil | Cdr Expression | Car Expression | Cons Expression Expression |

		Def Params Expression | RDef Name Params Expression | Clo Expression | Rec Name | 

		TRM Expression | TRCL Expression Expression Expression | RCL Expression Expression Expression | CNT Expression | TNT Expression | Skp

				deriving (Show, Eq, Ord)

type Name = String
type Params = [Name]

data AVal = AF Float | AI Integer | AC Char | AB Bool
		deriving (Show, Eq, Ord)

ts1 = Apply (Lambda (Apply (Lambda (BinOp Add (BinOp Mul (Variable "n") (Variable "m")) (Variable "m"))) (Value$AI 2))) (Value$AI 3)

ts2 = Apply (Lambda (BinOp Add (Value$AI 2) (Variable "x"))) (Value$AI 3)

ts3 = Apply (Lambda (BinOp Mul (Variable "x") (Value$AI 2)))
	(Apply (Lambda (BinOp Add (Variable "x") (Value$AI 2)))
	(Apply (Lambda (Apply (Lambda (BinOp Mul (Variable "n") (Variable "m"))) (Value$AI 2))) (Value$AI 3)))

ts4 = Cond (BinOp Lt (Value$AI 4) (Value$AI 3)) ts1 ts3

ts5 = Let "t" (Value$AI 2) (Cond (BinOp Lt (Variable "t") (Value$AI 3))  ts1 ts3)

ts6 = Apply (Lambda (BinOp Sub (Variable "n") (Value$AI 2))) (Apply (Lambda (Car (Variable "x"))) ls)

ls = Cons (Value$AI 2) Nil

em = Apply (Lambda (Apply (Lambda (Cons (Variable "x") (Variable "y"))) (Value$AI 3))) Nil

ts7 = Apply (Lambda (Apply (Lambda (Apply func (Value$AI 0))) (Value$AI 0))) trac

ts8 = Apply (Lambda (Apply (Lambda (Apply func (Value$AI 5))) (Value$AI 4))) (Value$AI 3)

func = Def  ["x", "y", "z"] (Lambda (BinOp Sub (Variable "x") (BinOp Sub (Variable "z") (Variable "y"))))

pfunc = RDef "pf" ["c", "a"]
	(RCL (BinOp Equ (Variable "c") (Value$AI 1)) 
	(TRM (Variable "a"))
	(CNT (Cons (BinOp Sub (Variable "c") (Value$AI 1)) (Cons (BinOp Mul (Variable "c") (Variable "a")) Nil))) )

fract = Apply (Lambda (Apply pfunc (Value$AI 6))) (Value$AI 1)


tfunc = RDef "pf" ["c", "a"]
	(RCL (BinOp Equ (Variable "c") (Value$AI 1)) 
	(TRM (Variable "a"))
	(TNT (Cons ( BinOp Sub (Variable "c") (Value$AI 1) ) (Cons (BinOp Mul (Variable "c") (Variable "a")) Nil))) )

trac = Apply (Lambda (Apply tfunc (Value$AI 69))) (Value$AI 1)

bin = Lambda$BinOp Add (Variable "x") (Variable "y")
--(Apply (Apply (Op ADD) (Variable "m")) (Variable "n"))

fuc = RDef "pf" ["c"]
	(RCL (BinOp Equ (Variable "c") (Value$AI 1)) 
	(TRM (Value$AI 1))
	(BinOp Mul (Variable "c") (CNT (Cons ( BinOp Sub (Variable "c") (Value$AI 1) ) Nil))) )

fct = Apply (Lambda (Apply fuc (Value$AI 6))) (Value$AI 1)


fib = RDef "f" ["n"]
	(RCL (BinOp Or (BinOp Equ (Variable "n") (Value$AI 0)) (BinOp Equ (Variable "n") (Value$AI 1)) )
	(TRM (Value$AI 1))
	(BinOp Add (CNT (Cons (BinOp Sub (Variable "n") (Value$AI 1)) Nil)) (CNT (Cons (BinOp Sub (Variable "n") (Value$AI 2)) Nil))))

frb = Apply fib (Value$AI 4)

lett = Let "f" (Value$AI 3) (BinOp Add (Variable "f") (Variable "f"))

cal = Apply (Lambda (BinOp Add (Variable "x") (Variable "x"))) (Value$AI 3)


brt = Apply (Lambda (BinOp Add (Variable "x") (Variable "x"))) cal

lamb = (Lambda (Apply (Lambda (BinOp Sub (Variable "x") (Variable "y"))) (Value$AI 3)))

lamp = Apply (Lambda (Apply (Variable "x") (Value$AI 2)))      (Lambda (Apply (Lambda (BinOp Sub (Variable "y") (Variable "x"))) (Value$AI 3)))

--     Apply (Lambda (Apply (Variable "x") (Value$AI 2)))      (Lambda (Apply (Lambda (BinOp Sub (Variable "y") (Variable ""))) (Value$AI 3)))

rty = Def ["a", "b"]
	(BinOp Add (Variable "a") (Variable "b"))


rtty = Apply (Lambda (Apply (Lambda (Apply (Lambda (Apply (Lambda rty) (Variable "x"))) (Variable "y"))) (Value$AI 3))) (Value$AI 2)


tnk = Apply (Lambda (Apply ( (Variable "x")) (Value (AI 2)))) (Lambda (BinOp Add (Variable "a") (Value$AI 3)))

tak = Apply (Lambda (Apply (Lambda (BinOp Sub (Variable "x") (Variable "y"))) (Value (AI 3)))) (Value$AI 2)


