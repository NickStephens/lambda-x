module Lambda where

-- This is similar to a Lambda bootstrap. The implementation has to
-- be based on something, which happens to be Haskell.

-- This module contains Lambda Calculus abstract data types and 
-- functions used for evaluating the Lambda Calculus


-- File to support lambda terms
-- closely based on Sergio Antoy's verion

type Variable = String

data Constant = Plus
              | Minus
              | Times
              | Div
              | BTrue
              | BFalse
              | Num Int
     deriving (Show, Eq)


data Exp = Lam Variable Exp
         | App Exp Exp
         | Var Variable
         | Cons Constant
  deriving Eq

instance Show Exp where
	show (Lam var exp)   = "\\" ++ var ++ " -> " ++ show exp
	show (App exp1 exp2) = "("++show exp1 ++ " " ++ show exp2++")"
	show (Var var)       = var
	show (Cons cons)     = show cons

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

-- ghci> nor (App (App y g) four
-- \f -> \x -> (f (f (f (f (f (f (f (f (f (f x))))))))))


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

app (Var x) = Var x
app (Lam x e) = Lam x (app e)
app (App e1 e2) = case app e1 of
	Lam x e -> let e2' = app e2 in app (sub e2' (Var x) e)
	e1'     -> let e2' = app e2 in App e1' e2'


cbv (Var x) = Var x
cbv (Lam x e) = Lam x e
cbv (App e1 e2) = case cbv e1 of
	Lam x e -> let e2' = cbv e2 in cbv (sub e2' (Var x) e)
	e1'     -> let e2' = cbv e2 in App e1' e2'




pair = Lam "a" (Lam "b" (Lam "f" (App (App (Var "f") (Var "a")) (Var "b"))))

first = Lam "p" (App (Var "p") true) --(Lam "a" (Lam "b" (Var "a"))))
second = Lam "p" (App (Var "p") false) --(Lam "a" (Lam "b" (Var "b"))))

empty = false

cons = Lam "h" (Lam "t" (Lam "c" (Lam "n"  (App (App (Var "c")(Var "h"))  (App (App (Var "t")(Var "c"))(Var "n"))))))

hed = Lam "l" (App (App (Var "l") true) false)

tale = Lam "l" (App first (App (App (Var "l") (Lam "a" (Lam "b" (App (App pair (App second (Var "b")))
			(App (App cons (Var "a"))(App second (Var "b"))))))) (App (App pair empty) empty)))

isEmpty = Lam "l" (App (App (Var "l") (Lam "a" (Lam "b" false))) true)

a =nor (App (App cons (Var "A")) empty)

b =nor (App (App cons (Var "B")) a)

c=nor (App (App cons (Var "C")) b) 

p = nor (App (App pair (Var "X"))(Var "Y"))

c' = nor (App (App cons (Var "A")) (App (App cons (Var "B")) (App (App cons (Var "C")) empty)))

zipped = nor (App (App (App y zip') c) c')

zip' = Lam "f"
	( Lam "l1" (Lam "l2" (App (App (App iF (App (App oR (App isEmpty (Var "l1"))) (App isEmpty (Var "l2"))))
	empty)
	(App (App cons
	(App (App pair (App hed (Var "l1"))) (App hed (Var "l2"))))
	(App (App (Var "f") (App tale (Var "l1"))) (App tale (Var "l2"))))
	)) )

ziip = Lam "a" (Lam "b" (App (App (App y zip') (Var "a")) (Var "b")))



-- sum of n
g = Lam "f" (Lam "n" (App (App (App iF (App isZero (Var "n"))) zero) (App (App add (Var "n"))
				(App (Var "f") (App (App subZ (Var "n")) one)))))

-- the Y combinator (!) 
y = Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x"))))
				(Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))

redex = App (App (Lam "x" (Lam "y" (App (App add (Var "x")) (Var "y"))))
	(App (Lam "z" (App suc (Var "z"))) four))
		(App (Lam "w" (App suc (Var "w"))) three) 

reex = App (App (Lam "x" (Lam "y" (App (App add (Var "x")) (Var "y"))))
	(App (Lam "z" (App (Var "z")(Var "z"))) (Var "one")))
		(App (Lam "w" (App (Var "w")(Var "w"))) (Var "two"))


bam = Lam "x" (App (Var "x") (Var "x")) 

zero = Lam "f" (Lam "x" (Var "x"))
one = Lam "f" (Lam "x" (App (Var "f") (Var "x")))
two = Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x"))))
three = Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))
four = App suc three

add = Lam "n" (Lam "m" (Lam "f" (Lam "x" (App (App (Var "n") (Var "f"))
                (App (App (Var "m") (Var "f"))
                     (Var "x"))))))


pow = Lam "n" (Lam "m" (Lam "f" (Lam "x"  (App (App (App (Var "m") (Var "n")) (Var "f")) (Var "x")))))

mul = Lam "f1" (Lam "f2" (Lam "f" (App (Var "f1") (App (Var "f2") (Var "f")))))

pre = Lam "n" (Lam "f" (Lam "x" (App (App (App (Var "n") (Lam "g" (Lam "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Lam "u" (Var "x")) ) (Lam "u" (Var "u")))))


subtrax = Lam "m" (Lam "n" (App (App (Var "n") pre) (Var "m")))

isZero  = Lam "n" (App (App (Var "n") (Lam "x" false)) true)
isZero' = Lam "x" (App (App (App (Var "x") false) noT) false)

gE = Lam "x" (Lam "y" (App isZero' (App (App (Var "x") pre) (Var "y"))))

eQ = Lam "x" (Lam "y" (App (App anD (App isZero' (App (App (Var "x") pre) (Var "y")))) (App isZero' (App (App (Var "y") pre) (Var "x")))))

eq = Lam "m" (Lam "n" (App (App anD (App (App ltE (Var "m")) (Var "n"))) (App (App ltE (Var "n")) (Var "m"))))

ltE = Lam "m" (Lam "n" (App isZero (App (App subtrax (Var "m")) (Var "n"))))

iF = Lam "m" (Lam "a" (Lam "b" (App (App (Var "m") (Var"a")) (Var "b"))))

subZ = Lam "m" (Lam "n" (App (App (App iF (App (App ltE (Var "m")) (Var "n"))) (App (App subtrax (Var "n")) (Var "m")) ) (App (App subtrax (Var "m")) (Var "n"))))
--App (App (App iF false) zero ) false

false = Lam "a" (Lam "b" (Var "b"))
true  = Lam "a" (Lam "b" (Var "a"))

anD = Lam "m" (Lam "n" (App (App (Var "m") (Var "n")) (Var "m")))
oR  = Lam "m" (Lam "n" (App (App (Var "m") (Var "m")) (Var "n")))
noT = Lam "m" (Lam "a" (Lam "b" (App (App (Var "m") (Var "b")) (Var "a"))))


suc = App add one

--(App (App subtrax (Var "n")) (Var "m"))) (App (App subtrax (Var "m")) (Var "n")))))
--
-- delta rules

delta (App (App (Cons Plus) (Cons (Num a))) (Cons (Num b))) = a + b

t1 = delta (App (App (Cons Plus) (Cons (Num 4))) (Cons (Num 5)))
   



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







