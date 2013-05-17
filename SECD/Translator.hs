import PCONS
import SEC
import AbstractSyntax
import qualified Data.Map as Map
import Control.Monad.State
import Data.List
import Parser
import Compiler
import Text.ParserCombinators.Parsec hiding (State)





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


funct p = case p of
	NoRec nm prms e -> do
		te <- trans e
		--translates alias w/ params into lambdas:
		return $ foldr (\a b -> Lambda [a] b) te prms
	Recr nm prms e -> do
			(env:es,_) <- get
			put $ (Map.insert nm 1 env:es, 1)
{-			case e of
				COND p e1 e2 -> do
					pred <- trans p
					trm <- trans e1
					cnt <- trans e2 --recont e2
					liftIO$print cnt
					let rec = Cond pred (TRM trm) cnt
					let arg = head prms
					let prms' = tail prms
					let recL = LetR (arg:[nm]) rec
					modify (\_ -> ([Map.empty],1))
					return $ foldr (\a b -> Lambda [a] b) recL prms'-}
--			te <- trans e
--			let arg = head prms
--			let prms' = tail prms
--			let rec = LetR (arg:[nm]) te
--			modify (\_-> ([Map.empty],1))
--			return $ foldr (\a b -> Lambda [a] b) rec prms'
			te <- trans e
			return $ LetR nm $ foldr (\a b -> Lambda [a] b) te prms



{-	TRec nm prms e -> do
		(env:es,_) <- get
		put $ (Map.insert nm 1 env:es, 2)
		case e of
			COND p e1 e2 -> do
				pred <- trans p
				trm <- trans e1
				cnt <- trans e2 --recont e2
				let rec = RCL pred (TRM trm) cnt
				let recL = foldr (\a b -> Lambda [a] b) rec prms
				modify (\_ -> ([Map.empty],1))
				return $ LetR nm recL --RDef prms rec-}
funcStream [p] = funct p
funcStream (p:ps) = do
	let nm = nameOf p
	cont <- funcStream ps
	fnc <- funct p
	return $ Apply (Lambda [nm] cont) fnc --Lett nm fnc cont
		where nameOf p = case p of
			NoRec n _ _ -> n
			Recr  n _ _ -> n
			TRec  n _ _ -> n		



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
			Op op -> return $ (opm op ty)
{-			Var v -> do
				(env:es,n) <- get
				if Map.member v env
					then do
						let cnt = if n==1 then CNT (Variable v) else TNT (Variable v)
						return $ cnt ty
					else return $ Apply (Variable v) ty-}
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
	Var v -> do
		(env:es,n) <- get
		if Map.member v env
			then do
				let cnt = if n==1 then CNT (Variable v) else TNT (Variable v)
				return $ cnt
			else return $ Variable v
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
-}
	Let a ex -> do
		tex <- trans ex
		case a of
			NoRec nm [] e -> do
				te <- trans e
				return $ Apply (Lambda [nm] tex) te --Lett nm te tex


valuate v = case v of
	ValInt i    -> Value$AI i
	ValDouble d -> Value$AD d
	ValBool b   -> Value$AB b
	ValChar c   -> Value$AC c


opm op ty = case op of
	SUB -> Lambda [""] $ BinOp Sub ty (Variable "")
	ADD -> Lambda [""] $ BinOp Add ty (Variable "")
	DIV -> Lambda [""] $ BinOp Div ty (Variable "")
	MUL -> Lambda [""] $ BinOp Mul ty (Variable "")
	CONSo -> Lambda [""] $ BinOp Cons ty (Variable "")
	LTo -> Lambda [""] $ BinOp Lt ty (Variable "")
	GTo -> Lambda [""] $ BinOp Gt ty (Variable "")
	EQo -> Lambda [""] $ BinOp Equ ty (Variable "")
	CARo -> UnOp Car ty
	CDRo -> UnOp Cdr ty


upm op = case op of
--	NOT -> Not
	CARo -> Car
	CDRo -> Cdr


translate expr = evalStateT (trans expr) ([Map.empty], 1)



