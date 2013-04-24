module SECD where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

type Scratch = [Value]
type Env     = [Value]
type Code   = [Instr]
type Dump    = [Store]
data Store   = BRT [Instr] | Call (Scratch, Env, Code) deriving Show

type Moment  = (Scratch, Env, Code)

data Value   = I Integer | L [Value] | Cl Closure | B Bool | E Env | Bl Block deriving (Show, Eq)
type Var     = String
type Closure = (Func, Env)
type Const   = Integer
type Block   = [Instr]
type Func    = [Instr]
data Oper    = Add | Sub | Mul | Div | Mod deriving (Show, Eq)
data Rela    = Lt | Gt | Equ deriving (Show, Eq)
data Instr =
			ACC Int |
			CLOS | LETREC | LETAILREC |
			LET |
			ENDLET |
			SEL |
			BL Block | --load func
			APP | RAP | TAP |
			RTN |
			LDC Value |
			Op Oper |
			Rel Rela |
			NIL | CONS | CAR | CDR | NULL deriving (Eq, Show)
			

{-
instance Show Value where
	show (I i) = show i
	show (L v) = show v
	show (Cl (f, e)) = "Cl ("++show f++",env)"
	show (B b) = show b
-}
delta :: Secd ()
delta = do
	instr <- topC
	(s, e, c) <- get
	case instr of
		LDC x -> put (x:s, e, c)
		ACC n -> do
			put (e!!(n-1):s, e, c)
		LET -> do
			let v = head s
			put (s, v:e, c)
		LETAILREC -> do
			let Bl c' = head s
			put (Cl (c', Bl c':[]):tail s, e, c)
		LETREC -> do
			let Bl c' = head s
			put (Cl (c',Bl c':e):tail s, e, c)
		ENDLET -> do
			popE
		SEL -> do
			let B bl = head s
			let (BL btr:BL bfl:cs) = c
			case bl of
				True  -> put (tail s, e, btr++cs)
				False -> put (tail s, e, bfl++cs)
		BL bl -> do
			put (Bl bl:s, e, c)
		CLOS -> do
			let Bl c' = head s
			put (Cl (c',e):tail s, e, c)
		APP -> do
	 		let (L v:Cl (c',e'):rest) = s
			put (Bl c:E e:rest, v++e', c')
		TAP -> do
			let (L v:Cl (c',e'):rest) = s
			put (rest, v++e', c')
		RTN -> do
			let (v:Bl c':E e':rest) = s
			put (v:rest, e', c')
		Op op -> do
			rslt <- oper op s
			put (rslt:((tail . tail) s), e, c) --pop two operands off S
		Rel rel -> do
			rslt <- rela rel s
			put (rslt:((tail . tail) s), e, c) --pop two operands off S
		CONS -> do
			let (a:L as:rest) = s
			put ((L (a:as)):rest, e, c)
		CAR -> do
			let L as = head s
			put (head as:tail s, e, c)
		CDR -> do
			let L as = head s
			put (L (tail as):tail s, e, c)
		NULL -> do
			let a = head s
			if a==L []
				then put (B True:tail s, e, c)
				else put (B False:tail s, e, c)
		NIL -> do
			put (L []:s, e, c)

--rplaca v = case v of
--	Cl (f,n:e) -> [Cl (f,(rplaca v):e)]


--oper :: Oper -> Scratch -> Secd Value
oper op s
	|length s < 2 = throwError "not enough values on stack to operate on"
	|otherwise = case head s of
		I i -> case head (tail s) of
			I i' -> return (appO op i i')
			_    -> throwError "second arg not an int"
		_   -> throwError "first arg not an int"

appO op i i'
	|op == Add = I $ i + i'
	|op == Sub = I $ i - i'
	|op == Mul = I $ i * i'
	|op == Div = I $ i `div` i'
	|op == Mod = I $ i `mod` i'

rela rel s
	|length s < 2 = throwError "not enough values on stack to operate on"
	|otherwise = case head s of
		I i -> case head (tail s) of
			I i' -> return (appR rel i i')
			_    -> throwError "second arg not an int"
		_   -> throwError "first arg not an int"

appR rel i i'
	|rel == Lt = B $ i < i'
	|rel == Gt = B $i > i'
	|rel == Equ = B $ i == i'

--type Secd = ErrorT String (StateT Moment IO)
type Secd = StateT Moment (ErrorT String IO)

run p = runtest ([], [], p)

run' :: Secd ()
run' = do
	(s,e,c)  <- get
	liftIO $ putStrLn ("S: " ++ (show s))
	liftIO $ putStrLn ("E: " ++ (show e))
	liftIO $ putStrLn ("C: " ++ (show c))
	delta
	liftIO $ putStrLn ""
	liftIO $ putStrLn ("Instr: " ++ (show$head c)++" ->")
	secd' <- get
	case secd' of
		(v, e, []) -> do
			liftIO $ putStrLn ("result: "  ++ (show  v))
		secd''         -> run'

displayEnv :: Env -> Secd ()
displayEnv e = do
	if null e
		then do
			liftIO $ putStrLn "   []"
			return ()
		else do
			liftIO $ putStrLn ("   "++ show e)
			displayEnv $tail e
--runtest tp = evalStateT (runErrorT run') tp
runtest tp = runErrorT (evalStateT run' tp)

t1 = [BL cl, CLOS, NIL, LDC (I 2),CONS, APP, BL cl, CLOS, NIL, LDC (I 2), CONS, APP, Op Add]
cl = [ACC 1, LDC (I 1), Op Add, RTN]

t2   = [fact, LETREC, NIL, LDC (I 1), CONS, LDC (I 5), CONS, APP]
fact = BL [ACC 1, LDC (I 1), Rel Equ, SEL,
	BL [ACC 2,RTN],
	BL [ACC 3, LETREC, NIL, ACC 1, ACC 2, Op Mul, CONS, LDC (I 1), ACC 1, Op Sub, CONS, APP],RTN]

t3   = [fact', LETAILREC, NIL, LDC (I 1), CONS, LDC (I 5), CONS, TAP]
fact' = BL [ACC 1, LDC (I 1), Rel Equ, SEL,
	BL [ACC 2],
	BL [ACC 3, LETAILREC, NIL, ACC 1, ACC 2, Op Mul, CONS, LDC (I 1), ACC 1, Op Sub, CONS, TAP]]
--Stack operations

pushS :: Value -> Secd ()
pushS v = do
	(s, e, c) <- get
	put (v:s, e, c)
	return ()

popS :: Secd ()
popS = do
	(s, e, c) <- get
	put (tail s, e, c)
	return ()

topS :: Secd Value
topS = do
	(s, e, c) <- get
	put (tail s, e, c)
	return $ head s

topC :: Secd Instr
topC = do
	(s, e, c) <- get
	put (s, e, tail c)
	return $ head c

popE :: Secd ()
popE = do
	(s, e, c) <- get
	put (s, tail e, c)
	return ()




