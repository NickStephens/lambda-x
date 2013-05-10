import PCONS
import SEC
import AbstractSyntax
import qualified Data.Map as Map
import Control.Monad.State
import Data.List
import Parser
import Compiler
import Text.ParserCombinators.Parsec hiding (State)



funct p = case p of
	NoRec nm prms e -> do
		te <- trans e
		return $ te
	Recr nm prms e -> do
			(env:es,_) <- get
			put $ (Map.insert nm 1 env:es, 1)
			case e of
				COND p e1 e2 -> do
					pred <- trans p
					trm <- trans e1
					cnt <- trans e2 --recont e2
					let rec = RCL pred (TRM trm) cnt
					modify (\_ -> ([Map.empty],1))
					return $ RDef prms rec
	TRec nm prms e -> do
		(env:es,_) <- get
		put $ (Map.insert nm 1 env:es, 2)
		case e of
			COND p e1 e2 -> do
				pred <- trans p
				trm <- trans e1
				cnt <- trans e2 --recont e2
				let rec = RCL pred (TRM trm) cnt
				modify (\_ -> ([Map.empty],1))
				return $ RDef prms rec
funcStream [p] = funct p
funcStream (p:ps) = do
	let nm = nameOf p
	cont <- funcStream ps
	fnc <- funct p
	return $ Lett nm fnc cont
		where nameOf p = case p of
			NoRec n _ _ -> n
			Recr  n _ _ -> n
			TRec  n _ _ -> n		





transl =  evalStateT trns ([Map.empty], 1)
trns = do
	prg <- mind "pecan.txt"
	trs <- funcStream prg
	return trs
compl = evalStateT silt ([Map.empty], 1)
silt = do
	prg <- mind "pecan.txt"
	trs <- funcStream prg
	cm  <- comp trs
	return cm

prog = evalStateT prg ([Map.empty],1)
prg = do
	prg <- mind "pecan.txt"
	p <- funcStream prg
	e <- comp p
--	liftIO $ print p
--	liftIO $ print e
	liftIO $ run e
	return ()

mind :: String -> TRN [Alias]
mind file = do
	f <- liftIO $ parseFromFile program file
	case f of
		Right res -> return res

pecan = evalStateT pcn ([Map.empty],1)
pcn = do
	prg <- mind "pecan.txt"
	liftIO$ print prg
	return ()

--recont e = do
{-
Letrec fact n = if (n==1) then (1) else (n*(fact ((n-1):[])));
main = fact 5;
-}

--(CNT (BinOp Cons (BinOp Sub (Variable "n") (Value (AI 1))) (LSD [])))
--Tletrec fact c a = if (n==1) then (a) else (fact ((c-1):(a*c):[]));


trans :: Expr -> TRN EXP
trans expr = case expr of
{-	App a b -> do
		b' <- trans b
		case a of
			App x y -> do
				y' <- trans y
				case x of
					Op op -> return $ BinOp (opm op) y' b'
					Var x -> do
						(env:es,n) <- get
						if Map.member x env
							then do
								let cnt = if n==1 then CNT else TNT
								return $ cnt b'
							else return $ Apply (Apply (Variable x) y') b'
					_ -> do
						x' <- trans x
						return $ Apply (Apply x' y') b'
			Op op -> return $ UnOp (upm op) b'
			Var x -> do
				(env:es,n) <- get
				if Map.member x env
					then do
						let cnt = if n==1 then CNT else TNT
						return $ cnt b'
					else return $ Apply (Variable x) b'
			_ -> do
				a' <- trans a
				return $ Apply a' b'
-}
	App x y -> do
		ty <- trans y
		case x of
			Op op -> return $ Lambda [""] (BinOp (opm op) ty (Variable ""))
			_     -> do
				tx <- trans x
				return $ Apply tx ty

	COND p e1 e2 -> do
		tp <- trans p
		te1 <- trans e1
		te2 <- trans e2
		return $ Cond tp te1 te2
	Val v -> return $ valuate v
	Lst l -> do
		ls <- mapM trans l
		return $ LSD ls
	Pr (x:[y]) -> do
		tx <- trans x
		ty <- trans y
		return $ PR (tx:[ty])
	Var n -> return $ Variable n
	--nested Lams are treated as a function with multiple params
	Lam x e -> do
		case e of
			Lam y f -> do
				Lambda nms ex <- trans e
				return $ Lambda (x:nms) (Lambda [] ex)
			_ -> do
				te <- trans e
				return $  Lambda [x] te

{-	Lam x e -> do
		te <- trans e
		return $ Lambda [x] te
	Let a ex -> do
		tex <- trans ex
		case a of
			NoRec nm [] e -> do
				te <- trans e
				return $ Lett nm te tex-}
{-	NoRec nm prms e -> do
		te <- trans e
		return $ Def prms te
	Recr nm prms e -> do
		case e of
			COND p e1 e2 -> do
				pred <- trans p
				trm <- trans e1
				cnt <- trans e2
				let rec = RCL pred (TRM trm) (CNT cnt)
				return $ RDef nm prms rec-}

valuate v = case v of
	ValInt i    -> Value$AI i
	ValDouble d -> Value$AD d
	ValBool b   -> Value$AB b
	ValChar c   -> Value$AC c


opm op = case op of
	SUB -> Sub
	ADD -> Add
	DIV -> Div
	MUL -> Mul
	CONSo -> Cons
	LTo -> Lt
	GTo -> Gt
	EQo -> Equ


upm op = case op of
--	NOT -> Not
	CARo -> Car
	CDRo -> Cdr


translate expr = evalStateT (trans expr) ([Map.empty], 1)







