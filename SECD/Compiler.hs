module Compiler where


import PCONS
import SEC
import qualified Data.Map as Map
import Control.Monad.State
import Data.List



type TRN = StateT ([Scope], Int) IO

type Scope = Map.Map String Int
type CP = State (Scope, Int)



comp :: EXP -> TRN [Code]
comp expr = case expr of
	Lambda x e -> do
		env <- get
		--x is a list of names that must be reversed to get the right ACC numberings
		params (reverse x)
		e' <- comp e
		put env
		return [BL (e' ++ [RTN]),CLOS]
	Apply f v -> do
		(e,_) <- get
		cf <- comp f
		(e,_) <- get
		cv <- comp v --LD
		return $ cf ++ cv ++ [APP]
	Variable x -> do
		(es,_) <- get
--		liftIO$print es
--		liftIO$ putStr (show x++" <- ")
		v <- deBruijn x es
--		liftIO$ print v
		return $ [ACC v]
	BinOp op e1 e2 -> do
		ce1 <- comp e1
		ce2 <- comp e2
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
		p <- comp pred
		th' <- comp th
		el' <- comp el
		return $ p ++ [SEL] ++ [BL th'] ++ [BL el']
--	Lett nm e1 e2 -> do
--		params [nm]
--		e1' <- comp e1
--		e2' <- comp e2
--		return $ e1' ++ [LET] ++ e2' ++ [ENDLET]
	LetR nm rec -> do
		params [nm]
		crec <- comp rec
		return [BL (crec ++ [RTN]),LETREC]
	CLst xs -> do
		xs' <- mapM comp xs
		let ls = concat $ intersperse [CONS] xs'
		return ls
	LSD l -> do
		ls <- mapM lstVal l
		return $ [LDC (L (concat ls))]
	PR (x,y) -> do
		[cx] <- lstVal x
		[cy] <- lstVal y
		return $ [LDC (P (cx,cy))]
	Nil -> return [NIL]
	Def ps e -> do
		params (reverse ps)
		ce <- comp e
		return [BL (ce ++ [RTN]),CLOS]
	RDef ps e -> do
		modify (\(e,_) -> (e,2))
		env <- get
		params ps
		ce <- comp e
		put env
		return ce
	TRM e -> do --terminate
		ce <- comp e
		return $ ce -- ++[RTN]
	CNT v -> do --continue
		cv <- comp v
		return $ cv --(cv ++ [LETREC])
	TNT e -> do --tail continue
		ce <- comp e
		return $ ce++[TAP] --leaves the superflous RTN in RCL
	FAULT -> do
		return $ [PATTERN_ERR]

deBruijn x (e:es) = do
	case Map.lookup x e of
		Nothing -> do
			v <- deBruijn x es
			return $ v+1
		Just v -> do
			return $ v

params [] = return ()
params ps = do
	modify (\(e,n) -> (Map.empty:e,1))
	mapM index ps
	return ()
index p = do
	(e:es,vn) <- get
	put (Map.insert p vn e:es, vn+1)

lstVal v = do
	case v of
		Value (AD d) -> return [D d]
		Value (AI i) -> return [I i]
		Value (AB b) -> return [B b]
		Value (AC c) -> return [C c]
		LSD (lsd) -> do
			lsd' <- mapM lstVal lsd
			return [L (concat lsd')]
		PR (pr1, pr2) -> do
			pr1' <- lstVal pr1
			pr2' <- lstVal pr2
			return [P (head $ pr1', head $ pr2')]


opt o = case o of
	PairIt -> PRC
	Car -> CAR
	Cdr -> CDR
	Cons -> CONS
	Fst -> FS
	Snd -> SN
	Not -> NOTOP
	_ -> OP o
