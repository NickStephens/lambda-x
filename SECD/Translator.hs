import PCONS
import SEC
import AbstractSyntax
import qualified Data.Map as Map
import Control.Monad.State
import Data.List
import Parser
import Compiler
import Text.ParserCombinators.Parsec hiding (State)
import Desugarer


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
	liftIO $ run e
	return ()

mind :: String -> TRN [DesugaredAlias]
mind file = do
	f <- liftIO $ parseFromFile program file
	case f of
		Right res -> return $ desugar res

pecan = evalStateT pcn ([Map.empty],1)
pcn = do
	prg <- mind "pecan.txt"
	liftIO$ print prg
	return ()


funct p = case p of
	DNoRec nm prms e -> do
		te <- trans e
		return $ foldr (\a b -> Lambda [a] b) te prms
	DRecr nm prms e -> do
			(env:es,_) <- get
			put $ (Map.insert nm 1 env:es, 1)
			te <- trans e
			return $ LetR nm $ foldr (\a b -> Lambda [a] b) te prms

funcStream [p] = funct p
funcStream (p:ps) = do
	let nm = nameOf p
	cont <- funcStream ps
	fnc <- funct p
	return $ Apply (Lambda [nm] cont) fnc
		where nameOf p = case p of
			DNoRec n _ _ -> n
			DRecr  n _ _ -> n
			DTRec  n _ _ -> n		

trans :: Expr -> TRN EXP
trans expr = case expr of

	App x y -> do
		ty <- trans y
		case x of
			Op op -> return $ (opm op ty)
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
	Pr (x,y) -> do
		tx <- trans x
		ty <- trans y
		return $ PR (tx,ty)
	Var v -> do
		(env:es,n) <- get
		if Map.member v env
			then do
				let cnt = if n==1 then CNT (Variable v) else TNT (Variable v)
				return $ cnt
			else return $ Variable v
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
				return $ Apply (Lambda [nm] tex) te
-}

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
	AND -> Lambda [""] $ BinOp And ty (Variable "")
	OR  -> Lambda [""] $ BinOp Or ty (Variable "")
	NOT -> UnOp Not ty
	FST -> UnOp Fst ty
	SND -> UnOp Snd ty

--upm op = case op of
--	NOT -> Not
--	CARo -> Car
--	CDRo -> Cdr


translate expr = evalStateT (trans expr) ([Map.empty], 1)
