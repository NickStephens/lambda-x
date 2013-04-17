module Lambda where

import Control.Monad.State

-- This is similar to a Lambda bootstrap. The implementation has to
-- be based on something, which happens to be Haskell.

-- This module contains Lambda Calculus abstract data types and 
-- functions used for evaluating the Lambda Calculus


-- File to support lambda terms
-- closely based on Sergio Antoy's verion

-- Need to create method of implementing Haskell builtins for 
-- exploiting its efficiency. 

type Variable = String

data Constant = Plus
              | Minus
              | Times
              | Div
              | BTrue
              | BFalse
              | Num Int
	      | Concat
	      | Append
     deriving (Show, Eq)


data Exp = Lam Variable Exp
         | App Exp Exp
         | Var Variable
         | Cons String
	 | NReduce Exp
	 | AReduce Exp
  deriving Eq


instance Show Exp where
	show (Lam var exp)   = "\\" ++ var ++ " -> " ++ show exp
	show (App exp1 exp2) = "("++show exp1 ++ " " ++ show exp2++")"
	show (Var var)       = var
	show (Cons cons)     = show cons
	show (NReduce e)     = "@normal(" ++ show e ++ ")"
	show (AReduce e)     = "@applicative(" ++ show e ++ ")"

-- fv(exp) returns the free vars

fv :: Exp -> [Variable]

fv (Lam v e) = setdiff (fv e) [v]
fv (App e1 e2) = un (fv e1) (fv e2)
fv (Var v) = [v]

-- create a fresh variable. Use the function maximum which
-- returns the maximum value of a list (lexocographic ordering).
-- then append a prime. Could also use minimum. Or some other
-- plan. Just create a new variable not present in the list

fresh vars = minimum vars ++ "'"

--
-- do beta substitution
-- sub m x e:  sub m for x in e
--

sub m (Var x) (Cons c)           = (Cons c)         -- constant rule
sub m (Var x) (Var v)                           
   | x == v                = m                -- variable rule
   | x /= v                = (Var v)
sub m (Var x) (App e1 e2)        = (App (sub m (Var x) e1) (sub m (Var x) e2))
                                   -- app rule
sub m (Var x) (Lam v e)
   | x == v                = (Lam v e)        -- abs rule, x is the bound var
   | notElem x (fv e)
     || notElem v (fv m)   = (Lam v (sub m (Var x) e))
                                              -- abs , x not free in e OR
                                              -- y not free in m
   | otherwise             = (Lam z (sub m (Var x) (sub (Var z) (Var v) e)) )
       where
         z = fresh (un (fv m) (fv e))


-- Beta reduction

reduce (Cons c) = (Cons c)
reduce (Var x) = (Var x)
-- if the leftmost term of an application is a lambda abstraction,
-- reduce it using the sub rule
reduce (App (Lam v e) e2) = sub e2 (Var v) e
-- if the leftmost term of an application is not a lambda abstraction,
-- try reducing it
reduce (App e1 e2) = App (reduce e1) (reduce e2)
-- if the leftmost term is an abstraction, reduce the body
reduce (Lam v e) = Lam v (reduce e)


reDuce exp
	|exp' == exp = exp
	|otherwise   = reDuce exp'
		where exp' = reduce exp

--http://www.itu.dk/people/sestoft/papers/sestoft-lamreduce.pdf

-- Int how many reductions it took to reach current expression
-- Exp current expression
-- State s a = State { runState :: s -> (a, s) }

-- Reduction Analytics
type Red = StateT Int IO
--norAnalyze :: Exp -> (Exp, Int)
norAnalyze exp = runStateT (norTrack exp) 0

norTrack :: Exp -> Red Exp --tate Int Exp
norTrack (Var x) = return $ Var x
norTrack (Lam x e) = do
			nextExp <- norTrack e
			return $ Lam x (nextExp)
norTrack (App e1 e2) = do
			increment
			nextExp <- cbnTrack e1
			case nextExp of
				Lam x e -> do
					liftIO $ print e2
					e2' <- norTrack e2
					norTrack (sub e2' (Var x) e)
				e1' -> do	
					e1'' <- norTrack e1'
				       	e2' <- norTrack e2
				       	return $ App e1'' e2'

--cbnAnalyze :: Exp -> (Exp, Int)
--cbnAnalyze exp = runState (cbnTrack exp) 0

-- call-by-name step reduction tracking
cbnTrack :: Exp -> Red Exp
cbnTrack (App e1 e2) = do
			nextExp <- cbnTrack e1
			case nextExp of
				Lam x e -> do 
					     increment  
					     cbnTrack (sub e2 (Var x) e)
				e1' -> return $ App e1' e2
cbnTrack x = return x

increment :: Red () 
increment = do
		cnt <- get
		put (cnt + 1)
		return ()


-- ghci> nor (App (App y g) four
-- \f -> \x -> (f (f (f (f (f (f (f (f (f (f x))))))))))

-- normal reduction order
nor (Var x) = Var x
nor (Lam x e) = Lam x (nor e)
nor (App e1 e2) = case cbn e1 of
	Lam x e -> nor (sub e2 (Var x) e)
	e1'     -> let e1'' = nor e1' in App e1'' (nor e2)



cbn (Var x) = Var x
cbn (Lam x e) = Lam x e
cbn (App e1 e2) = case cbn e1 of
	Lam x e -> cbn (sub e2 (Var x) e)
	e1'     -> App e1' e2

------------------------------------------------
-- ghci> app (App (App y g) four
-- <does not terminate...>

-- applicative reduction order
app (Var x) = Var x
app (Lam x e) = Lam x (app e)
app (App e1 e2) = case app e1 of
	Lam x e -> let e2' = app e2 in app (sub e2' (Var x) e)
	e1'     -> let e2' = app e2 in App e1' e2'


cbv (Var x) = Var x
cbv (Lam x e) = Lam x e
cbv (NReduce e) = nor e
cbv (AReduce e) = app e
cbv (App e1 e2) = case cbv e1 of
	Lam x e -> let e2' = cbv e2 in cbv (sub e2' (Var x) e)
	e1'     -> let e2' = cbv e2 in App e1' e2'



-- Support functions: set difference and union

setdiff [] _ = []
setdiff x [] = x
setdiff (x:xs) ys
   | elem x ys             = setdiff xs ys
   | otherwise             = x:(setdiff xs ys)

un x [] = x
un [] x = x
un (x:xs) ys
   | elem x ys             = un xs ys
   | otherwise             = x:(un xs ys)


