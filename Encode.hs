module Encode where

-- Will encode Lambda Calculus functions into a form 
-- readable by the purely syntactic system of the Lambda Calculus

-- This must be Haskell based, the Lambda Calculus is a syntactic system 
-- and symbols like 'x' or 'y' hold no special meaning and are unable to 
-- compared for equality

import Lambda
import LambdaCore
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe

type Env = Map.Map String Exp

--type RC = StateT (Env, Exp) IO
type RC = State (Env, Exp)

recode expr = fst$runState (rc expr) (Map.empty, zero)
rc :: Exp -> RC Exp
rc expr = do
	x <- enc expr
	let xx = cbv (App cbV x)
	(env, _) <- get
	dex <- deco xx
	return dex


encode expr = fst $ runState (enc expr) (Map.empty, zero)
enc :: Exp -> RC Exp
enc (Lam v e) = do
	(env, vn) <- get
	case Map.lookup v env of
		Nothing -> do
			let advn = nor (App sucs vn)
			put $ (Map.insert v vn env, advn)
			e' <- enc e
			return $ App (App pr one)
				(App (App pr vn) e')
		Just x -> do
			e' <- enc e
			return $ App (App pr one)
				(App (App pr x) e')
enc (App e1 e2) = do
	e1' <- enc e1
	e2' <- enc e2
	return $ App (App pr two)
		(App (App pr e1') e2')
enc (Var v) = do
	(env, vn) <- get
	case Map.lookup v env of
		Nothing -> do
			let advn = nor (App sucs vn)
			put $ (Map.insert v vn env, advn)
			return $ App (App pr three) vn
		Just x  -> return $ App (App pr three) x

deco :: Exp -> RC Exp
deco term = do
	let rest = nor (App scnd term)
	(env, _) <- get
	let env' = swap env
	case (nor (App frst term)) of
		Lam "f" (Lam "x" (App (Var "f") (Var "x"))) -> do
			e <- deco (nor (App scnd rest))
			let nm = nor (App frst rest)
			let nm' = fromJust $ Map.lookup nm env'
			return $ Lam nm' e
		Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x")))) -> do
			e1 <- deco (nor (App frst rest))
			e2 <- deco (nor (App scnd rest))
			return $ App e1 e2
		Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))) -> do
			let name = fromJust $ Map.lookup rest env'
			return $ Var name

swap env = Map.fromList $ map (\(a,b) -> (b,a)) $ Map.toList env

decode term = let rest = nor (App scnd term) in
	case (nor (App frst term)) of
		Lam "f" (Lam "x" (App (Var "f") (Var "x"))) ->
			Lam (n2v (nor (App frst rest))) (decode (nor (App scnd rest)))
		Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x")))) ->
			App (decode (nor (App frst rest))) (decode (nor (App scnd rest)))
		Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))) ->
			Var (n2v rest)
{-
decode term = do
	let exp = nor (App frst term)
	let rst = nor (App scnd term)
	case exp of
		one -> do
			let v = n2v (nor (App frst rst))
			e <- decode (nor (App scnd rest))
			return $ Lam v e
		two -> do
			e1 <- (decode (nor (App frst rest)))
			e2 <- (decode (nor (App scnd rest)))
			return $ App e1 e2
		three -> return Var (n2v rst)
-}

												
n2v (Lam "f" (Lam "x" (Var "x"))) = "a"
n2v (Lam "f" (Lam "x" (App (Var "f") rest))) = n2v (Lam "f" (Lam "x" rest)) ++ "'"



