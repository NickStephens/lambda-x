module Compiler where


import PCONS
import SEC
import qualified Data.Map as Map
import Control.Monad.State
import Data.List

type Scope = Map.Map String Int
type CP = State (Scope, Int)
--compile :: Expr -> [Instr]
compile expr = fst$runState (comp expr) (Map.empty, 1)

comp :: Expression -> CP [Code]
comp expr = case expr of
	Lambda e -> do
		e' <- comp e
--		modify (\(e,n) -> (e,1)) --DeBrujin reset
		modify (\(e,n) -> (Map.empty,1)) --total env. reset
		return [BL (e' ++ [RTN]),CLOS]
	Apply f v -> do
		cf <- comp f --BL;CLOS
		cv <- comp v --LD
		return $ cf ++ ([NIL]++cv++[CONS]) ++ [APP]
	Variable x -> do
		(env, vn) <- get
		case Map.lookup x env of
			Nothing -> do
				put $ (Map.insert x vn env, vn+1)
				return [ACC vn]
			Just v -> do
				return [ACC v]
	BinOp op e1 e2 -> do
		ce1 <- comp e1
		ce2 <- comp e2
		return $ ce1 ++ ce2 ++ [OP op]
--		case e1 of
--			Var "" -> return $ ce1 ++ ce2 ++ [OP op]
--			_ -> case e2 of
--				Var "" -> return $ ce2 ++ ce1 ++ [OP op]
--				_ -> return $ ce1 ++ ce2 ++ [OP op]
--	UnOp op e -> do
--		ce <- comp e
--		return $ 
	Value v -> case v of
		AF f -> return [LDC (F f)]
		AI i -> return [LDC (I i)]
		AB b -> return [LDC (B b)]
		AC c -> return [LDC (C c)]
	Cond pred th el -> do
		p <- comp pred
		th' <- comp th
		el' <- comp el
		return $ p ++ [SEL] ++ [BL th'] ++ [BL el']
	Let nm e1 e2 -> do
		modify (\(e, v) -> (Map.insert nm v e, v+1))
		e1' <- comp e1
		e2' <- comp e2
		return $ e1' ++ [LET] ++ e2' ++ [ENDLET]
	CLst xs -> do
		xs' <- mapM comp xs
		let ls = concat $ intersperse [CONS] xs'
		return ls
	Nil -> return [NIL]
	Car e -> do
		ce <- comp e
		return $ ce ++ [CAR]
	Cdr e -> do
		ce <- comp e
		return $ ce ++ [CDR]
	Cons e1 e2 -> do
		ce1 <- comp e1
		ce2 <- comp e2
		return $ ce2 ++ ce1 ++ [CONS]
	Def ps e -> do --Def nm ps e -> do
--		modify (\(e, _) -> (e,1))
		params ps
		--modify $ \(e, v) -> (Map.insert nm v e, v+1) -- necessary? 
		ce <- comp e
		return ce
	RDef nm ps e -> do
		modify (\(e, _) -> (e,1))
		modify $ \(e, v) -> (Map.insert nm v e, v+1)
		params ps
		ce <- comp e
		return ce

	Skp -> return [SKP]
	RCL pred th el -> do
		p <- comp pred
		trm <- comp th
		cnt <- comp el
		return [BL [RC (p++[SEL]++[BL (trm++[RTN])]++[BL (cnt++[RTN])])],CLOS]
	TRM e -> do
		ce <- comp e
		return $ ce
	CNT e -> do
		ce <- comp e
		return $ ce++[RAP]
	TNT e -> do
		ce <- comp e
		return $ ce++[TAP] --leaves the superflous RTN in RCL

params [] = return ()
params (p:ps)= do
	modify $ \(env, vn) -> (Map.insert p vn env, vn+1)
	params ps


runt p = let p' = compile p in runtest ([], [], p')










