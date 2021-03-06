module SECD

import qualified Data.Map as Map
import LambdaCore


type S = [Value]
type E = [Env]
type C = [Directive]
type D = [(S,E,C)]
type Variable = String
data Directive = Term Exp | Apply
data Value =
	I Int | 
	Clos Env Variable Exp |
	Succ
type Env = Map.Map String Value
data Exp =
	Cons Int |
	Var Variable |
	Lam Variable Exp |
	App Exp Exp


run :: S -> E -> C -> D -> Value
run (v:[])	e'	[]	[] = v
run (v:[])	e'	[]	((s, e, c):d) = run (v:s) e c d
run s		e	(Term (Cons n):c)	d = run ((INT  n):s) e c d
run s		e	(Term (Var x):c)	d = run ((Map.lookup x e):s e c d
run s		e	(Term (Lam x t):c	d = run ((Clos e x t):s) e c d
run s		e	(Term (App t1 t2):c) d = run s e (Term t1):(Term t2):Apply:c d
run (Succ:(I n):s e (Apply:c)	d = run (I (n+1):s) e c d
run (Clos e' x t:v':s)	e	(Apply:c)	d = run [] (Map.insert x v' e') (Term t:[]) ((s, e, c):d)












