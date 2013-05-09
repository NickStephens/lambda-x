import PCONS
import SEC
import AbstractSyntax
import qualified Data.Map as Map
import Control.Monad.State
import Data.List
import Parser
import Compiler
import Text.ParserCombinators.Parsec hiding (State)





funcStream [p] = case p of
	NoRec nm prms e -> do
		te <- trans e
		return $ te
	Recr nm prms e -> do
		case e of
			COND p e1 e2 -> do
				pred <- trans p
				trm <- trans e1
				cnt <- trans e2 --recont e2
				let rec = RCL pred (TRM trm) (CNT cnt)
				return $ Def prms rec
funcStream (p:ps) = do
	cont <- funcStream ps
	case p of
		NoRec nm prms e -> do
			te <- trans e
			return $ Lett nm (Def prms te) cont
 
		Recr nm prms e -> do
			(env,_) <- get
			put $ (Map.insert nm 1 env, 1)
			case e of
				COND p e1 e2 -> do
					pred <- trans p
					trm <- trans e1
					cnt <- trans e2 --recont e2
					let rec = RCL pred (TRM trm) cnt
					modify (\_ -> (Map.empty,1))
					return $ Lett nm (RDef prms rec) cont

		TRec nm prms e -> do
			(env,_) <- get
			put $ (Map.insert nm 1 env, 2)
			case e of
				COND p e1 e2 -> do
					pred <- trans p
					trm <- trans e1
					cnt <- trans e2 --recont e2
					let rec = RCL pred (TRM trm) cnt
					modify (\_ -> (Map.empty,1))
					return $ Lett nm (RDef prms rec) cont




transv v = evalStateT (trv v) (Map.empty,1)
trv v = do
	tr <- trans v
	return tr
transl =  evalStateT trns (Map.empty, 1)
trns = do
	prg <- mind "pecan.txt"
	trs <- funcStream prg
	return trs
compl = evalStateT silt (Map.empty, 1)
silt = do
	prg <- mind "pecan.txt"
	trs <- funcStream prg
	cm  <- comp trs
	return cm

stilt ts = evalStateT (stm ts) (Map.empty, 1)
stm ts = do
	cm <- comp ts
	return cm



--recont e = do
{-
BinOp Mul (Variable "n") (Apply (Variable "fact") (BinOp Sub (Variable "n") (Value (AI 1))))

[ACC 1,NIL,LDC (I 1),ACC 1,OP Sub,CONS,APP,ACC 1,OP Mul]

(BinOp Mul (Variable "c") (CNT (Cons ( BinOp Sub (Variable "c") (Value$AI 1) ) Nil)))


[LDC (I 1),ACC 1,OP Sub,NIL,CONS,RAP,ACC 1,OP Mul]
-}

prog = evalStateT prg (Map.empty,1)
prg = do
	prg <- mind "pecan.txt"
	p <- funcStream prg
	e <- comp p
--	liftIO $ print p
--	liftIO $ print e
	liftIO $ run e
	return ()
--(CNT (BinOp Cons (BinOp Sub (Variable "n") (Value (AI 1))) (LSD [])))
--Tletrec fact c a = if (n==1) then (a) else (fact ((c-1):(a*c):[]));


trans :: Expr -> TRN EXP
trans expr = case expr of
	App a b -> do
		b' <- trans b
		case a of
			App x y -> do
				y' <- trans y
				case x of
					Op op -> return $ BinOp (opm op) y' b'
					Var x -> do
						(env,n) <- get
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
				(env,n) <- get
				if Map.member x env
					then do
						let cnt = if n==1 then CNT else TNT
						return $ cnt b'
					else return $ Apply (Variable x) b'
			_ -> do
				a' <- trans a
				return $ Apply a' b'

{-	App x y -> do
		ty <- trans y
		case x of
			Op op -> return $ Lambda [""] (BinOp (opm op) ty (Variable ""))
			_     -> do
				tx <- trans x
				return $ Apply tx ty
-}
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


translate expr = evalStateT (trans expr) (Map.empty, 1)

stamp = evalStateT cmpl (Map.empty,1)
camp = evalStateT cmp (Map.empty,1)

cmp :: TRN ()
cmp = do
	prg <- mind "pecan.txt"
	case head prg of
		NoRec _ _ ex -> do
			tr <- trans ex
			liftIO $ print tr
			liftIO $ putStrLn ""
			cp <- comp tr
			liftIO $ print cp
			return ()
		Recr _ _ ex -> do
			tr <- trans ex
			liftIO $ print tr
			liftIO $ putStrLn ""
			cp <- comp tr
			liftIO $ print cp
			return ()

mind :: String -> TRN [Alias]
mind file = do
	f <- liftIO $ parseFromFile program file
	case f of
		Right res -> return res

cmpl :: TRN ()
cmpl = do
	prg <- mind "pecan.txt"
	case head prg of
		NoRec _ _ ex -> do
			tr <- trans ex
			cp <- comp tr
			liftIO$ run cp
			return ()

tranny = evalStateT trny (Map.empty, 1)

trny = do
	prg <- mind "pecan.txt"
	case head prg of
		NoRec _ _ ex -> do
			tr <- trans ex
			liftIO$ print tr
			return ()


pecan = evalStateT pcn (Map.empty,1)
pcn = do
	prg <- mind "pecan.txt"
	liftIO$ print prg
	return ()





