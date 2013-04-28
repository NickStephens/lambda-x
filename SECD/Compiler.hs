import PCONS
import SEC
import qualified Data.Map as Map
import Control.Monad.State
--import Data.Maybe

type Scope = Map.Map String Int
type CP = State (Scope, Int)
--compile :: Expr -> [Instr]
compile expr = fst$runState (comp expr) (Map.empty, 1)

comp :: Expr -> CP [Code]
comp expr = case expr of
	Lam e -> do
		e' <- comp e
		return $ [BL (e' ++ [RTN]),CLOS]
	App f v -> do
		modify (\(e,n) -> (e,1))
		cf <- comp f
		cv <- comp v
		return $ cf ++ ([NIL]++cv++[CONS]) ++ [APP]
	Var x -> do
		(env, vn) <- get
		case Map.lookup x env of
			Nothing -> do
				put $ (Map.insert x vn env, vn+1)
				return $ [ACC vn]
			Just v -> do
				return $ [ACC v]
	BinOp op e1 e2 -> do
		ce1 <- comp e1
		ce2 <- comp e2
		return $ ce1 ++ ce2 ++ [Op op]
--	UnOp op e -> do
--		ce <- comp e
--		return $ 
	Val v -> case v of
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
	Nil -> return [NIL]
	Lst xs -> do
		xs' <- mapM comp xs
		let ls = concat xs'
		return $ [LDC (L ls)]
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




runt p = let p' = compile p in runtest ([], [], p')










