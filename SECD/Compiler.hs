module Compiler where


import PCONS
import SEC
import qualified Data.Map as Map
import Control.Monad.State
import Data.List



type TRN = StateT (Scope, Int) IO

type Scope = Map.Map String Int
type CP = State (Scope, Int)


comp :: EXP -> TRN [Code]
comp expr = case expr of
	Lambda x e -> do
		--x is a list of names that must be reversed to get the right ACC numberings
		params (reverse x)
		e' <- comp e
		modify (\(e,n) -> (Map.empty,1))
		return [BL (e' ++ [RTN]),CLOS]
	Apply f v -> do
		env <- get --preserve env to return
		modify (\(e,n) -> (Map.empty,1))
		cf <- comp f --BL;CLOS
--		modify (\(e,n) -> (Map.empty,1))
		put env --return to previous env
		cv <- comp v --LD
--		put env --return to previous env
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
--		env <- get --preserve env to return
--		modify (\(e,n) -> (Map.empty,1))
		ce1 <- comp e1
--		modify (\(e,n) -> (Map.empty,1))
		ce2 <- comp e2
--		put env --return to previous env
		return $ ce2 ++ ce1 ++ [opt op]
	UnOp op e -> do
		te  <- comp e
		return $ te ++ [opt op]
	Value v -> case v of
		AD d -> return [LDC (D d)]
		AI i -> return [LDC (I i)]
		AB b -> return [LDC (B b)]
		AC c -> return [LDC (C c)]
	Cond pred th el -> do
		env <- get
		modify (\(e,n) -> (Map.empty,1))
		p <- comp pred
		modify (\(e,n) -> (Map.empty,1))
		th' <- comp th
		modify (\(e,n) -> (Map.empty,1))
		el' <- comp el
		put env
		return $ p ++ [SEL] ++ [BL th'] ++ [BL el']

	ConS e1 e2 -> do
		env <- get
		modify (\(e,n) -> (Map.empty,1))
		ce1 <- comp e1
		modify (\(e,n) -> (Map.empty,1))
		ce2 <- comp e2
		put env
		return $ ce1++ce2++[CONS]
	Lett nm e1 e2 -> do
		env <- get
		modify (\(e,n) -> (Map.empty,1))
		e1' <- comp e1
		modify (\(e, v) -> (Map.insert nm v e, v+1))
		e2' <- comp e2
		put env
		return $ e1' ++ [LET] ++ e2' ++ [ENDLET]
	CLst xs -> do
		xs' <- mapM comp xs
		let ls = concat $ intersperse [CONS] xs'
		return ls
	LSD l -> do
		ls <- mapM lstVal l
		return $ [LDC (L (concat ls))]
	PR p -> do
		pr <- mapM lstVal p
		return $ [LDC (L (concat pr))]
	Nil -> return [NIL]
	Def ps e -> do
		params (reverse ps)
		ce <- comp e
		modify (\(e,n) -> (Map.empty,1))
		return [BL (ce ++ [RTN]),CLOS]

--		params (reverse x)
--		e' <- comp e
--		modify (\(e,n) -> (Map.empty,1))
--		return [BL (e' ++ [RTN]),CLOS]
	RDef ps e -> do
		env <- get
		modify (\(e, _) -> (Map.empty,2))
		params ps
		ce <- comp e
		put env
		return ce

	Skp -> return [SKP]
	RCL pred th el -> do --recursive call
		env <- get
		modify (\(e, _) -> (Map.empty,2))
		p <- comp pred
		modify (\(e, _) -> (Map.empty,2))
		trm <- comp th
		modify (\(e, _) -> (Map.empty,2))
		cnt <- comp el
		put env
		return [BL [RC (p++[SEL]++[BL (trm++[RTN])]++[BL (cnt++[RTN])])],CLOS]
	TRM e -> do --terminate
		env <- get
		modify (\(e, _) -> (Map.empty,2))
		ce <- comp e
		put env
		return $ ce
	CNT e -> do --continue
		env <- get
		modify (\(e, _) -> (Map.empty,2))
		ce <- comp e
		put env
		return $ ce++[RAP]
	TNT e -> do --tail continue
		env <- get
		modify (\(e, _) -> (Map.empty,2))
		ce <- comp e
		put env
		return $ ce++[TAP] --leaves the superflous RTN in RCL

params [] = return ()
params (p:ps)= do
	modify $ \(env, vn) -> (Map.insert p vn env, vn+1)
	params ps

lstVal v = do
	case v of
		Value (AD d) -> return [D d]
		Value (AI i) -> return [I i]
		Value (AB b) -> return [B b]
		Value (AC c) -> return [C c]

opt o = case o of
	Car -> CAR
	Cdr -> CDR
	Cons -> CONS
	_ -> OP o

--runt p = let p' = compile p in runtest ([], [], p')










