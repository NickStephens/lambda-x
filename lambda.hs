-- File to support lambda terms
-- closely based on Sergio Antoy's verion

--module lambda where

type Var = String

data Constant = Plus
              | Minus
              | Times
              | Div
              | BTrue
              | BFalse
              | Num Int
     deriving (Show, Eq)


data Exp = Abs Var Exp   -- \x -> (\y -> exp) == Abs (Var "x") (Abs (Var "y") exp)
         | App Exp Exp
         | Var Var
         | Cons Constant
  deriving (Eq)

instance Show Exp where
	show (Abs v e) = "\\" ++ v ++ ". " ++ show e
	show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
	show (Var var) = var
	show (Cons cons) = show cons

-- fv(exp) returns the free vars

fv :: Exp -> [Var]

fv (Abs v e) = setdiff (fv e) [v]
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
sub m (Var x) (Abs v e)
   | x == v                = (Abs v e)        -- abs rule, x is the bound var
   | notElem x (fv e)
     || notElem v (fv m)   = (Abs v (sub m (Var x) e))
                                              -- abs , x not free in e OR
                                              -- y not free in m
   | otherwise             = (Abs z (sub m (Var x) (sub (Var z) (Var v) e)) )
       where
         z = fresh (un (fv m) (fv e))


-- Beta reduction
-- I think this equivalent to one step in (leftmost?) outermost reduction

reduce (Cons c) = (Cons c)
reduce (Var x) = (Var x)
-- if the leftmost term of an application is a lambda abstraction,
-- reduce it using the sub rule
reduce (App (Abs v e) e2) = sub e2 (Var v) e
-- if the leftmost term of an application is not a lambda abstraction,
-- try reducing it
reduce (App e1 e2) = App (reduce e1) (reduce e2)
-- if the leftmost term is an abstraction, reduce the body
reduce (Abs v e) = Abs v (reduce e)

b_reduce :: Exp -> Maybe Exp
b_reduce (App (Abs v e) t) = return $ sub t (Var v) e
b_reduce _ = Nothing

outer (Var x) = Nothing
outer (Cons x) = Nothing
outer (Abs v e) = case (outer e) of
			Nothing -> Nothing
			Just e' -> return $ Abs v e'
outer t@(App e1 e2) = case (b_reduce t) of
			Nothing -> case (outer e1) of
					Nothing -> case (outer e2) of
							Nothing -> Nothing
							Just e2' -> return $ App e1 e2' 
					Just e1' -> return $ App e1' e2
			Just t' -> return t'

inner (Var x) = Nothing
inner (Cons x) = Nothing 
inner (Abs v e) = case (inner e) of
			Nothing -> Nothing
			Just e' -> return $ Abs v e'
inner t@(App e1 e2) = case (inner e1) of
			Nothing -> case (inner e2) of
					Nothing -> case (b_reduce t) of
							Nothing -> Nothing
							Just t' -> return t'
					Just e2' -> return $ App e1 e2'
			Just e1' -> return $ App e1' e2

eval strat t = case (strat t) of
		Nothing -> return t
		Just t' -> eval strat t'

{- An earlier attempt at the reduction strategies
leftOuter :: Exp -> Exp
leftOuter (Cons c) = (Cons c)
leftOuter (Var v) = (Var v)
leftOuter (App (Var v) e2) = (App (Var v) (leftOuter e2)) -- If the leftmost thing has already been reduced
--leftOuter (App (Abs v e) (App a1 a2)) = (App (Abs v e) (leftOuter (App a1 a2))) -- reduce the application we're applying the abstraction to.  (Maybe I should just reduce it?

leftOuter (App (Abs v e) e2) = reduce (App (Abs v e) e2)
leftOuter (App e1 e2) = (App (leftOuter e1) e2)
leftOuter (Abs v e) = (Abs v (leftOuter e))

cbn (App (Abs v e) e2) = cbn $ leftOuter (App (Abs v e) e2)
cbn (App (Var v1) (Var v2)) = (App (Var v1) (Var v2))
cbn (App (Var v) e2) = (App (Var v) (cbn e2)) -- If it's not an abstractions app
cbn (App e1 e2) = cbn $ leftOuter (App e1 e2)
cbn (Abs v e) = (Abs v (cbn e))
cbn a = a
-}

{-
leftInner :: Exp -> Exp
leftInner (App (App e1 e2) e3) = App (leftInner (App e1 e2)) e3
leftInner (App e1 (App e2 e3)) = App e1 (leftInner (App e2 e3))
leftInner t = reduce t

cbv (App (Abs v e) e2) = cbv $ leftInner (App (Abs v e) e2)
cbv (App (Var v1) (Var v2)) = (App (Var v1) (Var v2))
cbv (App (Var v) e2) = (App (Var v) (cbv e2)) 
cbv (App e1 e2) = cbv $ leftInner (App e1 e2)
cbv (Abs v e) = (Abs v (cbv e))
cbv a = a
-}

-- Test cases: 
{-
*Main> cbn (App throw (App self_app self_app))
Var "z"
*Main> cbv (App throw (App self_app self_app))

^CInterrupted.
*Main>
-}

-- The Y Combinator
y = Abs "h" (App (Abs "x" (App (Var "h") (App (Var "x") (Var "x")))) (Abs "x" (App (Var "h") (App (Var "x") (Var "x")))))

fac = (Abs "f" (Abs "n" (App (App (App if_ (App iszero (Var "n"))) one) (App (App mul (Var "n")) (App (Var "f") (App pre (Var "n")))))))

-- Church numerals: the number of apps of f corresponds to the
-- numeral

zero = Abs "f" (Abs "x" (Var "x"))
one = Abs "f" (Abs "x" (App (Var "f") (Var "x")))
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))
three = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))
four = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))

add = Abs "n" (Abs "m" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f"))
                (App (App (Var "m") (Var "f"))
                     (Var "x"))))))

-- iszero: returns true if the n is zero
iszero  = Abs "n" (App (App (Var "n") (Abs "x" false)) true)

-- minus: subtract n from m (note: if m < n then 0)
minus = Abs "m" (Abs "n" (App (App (Var "n") pre) (Var "m")))

-- predecessor: subtracts one
pre = Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x")) ) (Abs "u" (Var "u")))))

true = Abs "x" (Abs "y" (Var "x"))
false = Abs "x" (Abs "y" (Var "y"))

-- if: simulates and if then else
if_ = Abs "f" (Abs "x" (Abs "y" (App (App (Var "f") (Var "x")) (Var "y"))))

-- multiplication: an implementation of mul using the Y combinator
mul_rec = Abs "f" (Abs "n" (Abs "m" (App (App (App if_ (App iszero (Var "m"))) zero) (App (App add (Var "n")) (App (App (Var "f") (Var "n")) (App (App minus (Var "m")) one))))))

-- multiplication: a non-recursive mul
mul = Abs "m" (Abs "n" (Abs "x" (App (Var "m") (App (Var "n") (Var "x")))))

-- \n -> \m -> \f -> \x -> (n f) (m f) x

-- Main> reduce (App (App add one) one)
-- App (Abs "m" (Abs "f" (Abs "x" (App (App (Abs "f" (Abs "x" (App (Var "f") (Var "x")))) (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))) (Abs "f" (Abs "x" (App (Var "f") (Var "x"))))
-- Main> reduce (reduce (App (App add one) one))
-- Abs "f" (Abs "x" (App (App (Abs "f" (Abs "x" (App (Var "f") (Var "x")))) (Var "f")) (App (App (Abs "f" (Abs "x" (App (Var "f") (Var "x")))) (Var "f")) (Var "x"))))
-- Main> reduce (reduce (reduce (App (App add one) one)))
-- Abs "f" (Abs "x" (App (Abs "f'" (App (Var "f") (Var "f'"))) (App (Abs "f'" (App(Var "f") (Var "f'"))) (Var "x"))))
-- Main> reduce (reduce (reduce (reduce (App (App add one) one))))
-- Abs "f" (Abs "x" (App (Var "f") (App (Abs "f'" (App (Var "f") (Var "f'"))) (Var"x"))))
-- Main> reduce (reduce (reduce (reduce (reduce (App (App add one) one)))))
-- Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))
-- Main>

-- reduce (add one one). This takes 5 reductions
-- Main> reduce (reduce (reduce (reduce (reduce (App (App add one) one)))))
-- Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))
-- Main>



suc = App add one


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

-- Adding 3 and 4 using Church-Numerals

add_4_3 = App (App (Abs "n" (Abs "m" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f"))
                (App (App (Var "m") (Var "f"))
                     (Var "x"))))))) (Abs "f'" (Abs "x'" (App (Var "f'") (App 
					 	(Var "f'") (App (Var "f'") (App (Var "f'") (Var "x")))))))) --four
						(Abs "g" (Abs "y" (App (Var "g") (App (Var "g") (App 
						(Var "g") (Var "y")))))) -- three

-- \x -> \y -> x True
-- \x -> \y -> y False

-- \cond -> \then -> \else -> cond then else  -- apply condition, a boolean described above to then and else!

-- \f . \x . \y . (f x) y

-- equals needs to result in a lambda abstraction which is equivalent to a boolean

-- \m . \n . \x . \y .  

{-
data Exp = Abs Var Exp   -- \x -> (\y -> exp) == Abs (Var "x") (Abs (Var "y") exp)
         | App Exp Exp
         | Var Var
         | Cons Constant
  deriving (Show, Eq)
-}

-- leftmost innermost reduction
-- reduces the leftmost innermost redex
-- equivalent to call-by-value

self_app = Abs "x" (App (Var "x") (Var "x"))

throw = Abs "y" (Var "z")
