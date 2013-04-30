module SEC where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

type Scratch = [Code]
type Env     = [Code]
type CodeS   = [Code]


type Continuation  = (Scratch, Env, CodeS)


type Var     = String
type Closure = (Func, Env)
type Const   = Integer
type Block   = [Code]
type Func    = [Code]
data Oper    = Add | Sub | Mul | Div | Mod | Not | Neg | Lt | Gt | Equ
		deriving (Show, Eq, Ord)
data Code =
			ACC Int |
			CLOS | LTRC | TLTRC | STOS Block |
			LET |
			ENDLET |
			SEL |
			BL Block | --load func
			RC Block |
			APP | TAP | RAP |
			RTN |
			LDC Code |
			Op Oper |
			NIL | CONS | CAR | CDR | NULL |
			I Int | F Float | L [Code] | CL Closure | B Bool |
			E Env | C Char 
				deriving (Eq, Show)
{-
instance Show Code where
	show (I i) = show i
	show (L v) = show v
	show (CL (f, e)) = "CL ("++show f++", "++show e
	show (B b) = show b
	show (E env) = show env
	show (BL block) = "BL "++show block
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
			put (tail s, v:e, c)
		TLTRC -> do
			let BL c' = head s
			put (CL (c', BL c':[]):tail s, e, c)
		LTRC -> do
			let CL (c', _) = head s
			put (CL (c',BL c':e):tail s, e, c)
		ENDLET -> do
			popE
		SEL -> do
			let B bl = head s
			let (BL btr:BL bfl:cs) = c
			case bl of
				True  -> put (tail s, e, btr++cs)
				False -> put (tail s, e, bfl++cs)
		BL bl -> do
			put (BL bl:s, e, c)
		CLOS -> do
			let BL c' = head s
			put (CL (c',e):tail s, e, c)
		APP -> do
	 		let (L v:CL (c',e'):rest) = s
			put (BL c:E e:rest, v++e', c')
		RTN -> do
			let (v:BL c':E e':rest) = s
			put (v:rest, e', c')
		Op op -> do
			rslt <- oper op s
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
		RC c' -> do
			put (s, RC c':e, c'++c)
		RAP -> do
			let (L as:rest) = s
			let (RC c':e')  = e
			put (BL c:E e':rest, RC c':as, c')
		TAP -> do
			let (L as:rest) = s
			let (RC c':e')  = e
			put (rest, RC c':as, c')



oper :: Oper -> Scratch -> Secd Code
oper op s
	|length s < 2 = throwError "not enough values on stack to operate on"
	|otherwise = case head s of
		I i -> case head (tail s) of
			I i' -> return (appO op i i')
			_    -> throwError "second arg not an int"
		_   -> throwError "first arg not an int"

appO op i i'
	|op == Add = I $ i' + i
	|op == Sub = I $ i' - i
	|op == Mul = I $ i' * i
	|op == Div = I $ i' `div` i
	|op == Mod = I $ i' `mod` i
	|op == Lt = B $ i < i'
	|op == Gt = B $i > i'
	|op == Equ = B $ i == i'

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

type Secd = StateT Continuation (ErrorT String IO)

run p = runtest ([], [], p)

run' :: Secd ()
run' = do
	(s,e,c)  <- get
	liftIO $ putStrLn ("S: " ++ (show s))
	liftIO $ putStrLn ("E: " ++ (show e))
	liftIO $ putStrLn ("C: " ++ (show c))
	delta
	liftIO $ putStrLn ""
	liftIO $ putStrLn ("Code: " ++ (show$head c)++" ->")
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

runtest tp = runErrorT (evalStateT run' tp)


t3   = [fact', TLTRC, NIL, LDC (I 1), CONS, LDC (I 5), CONS, TAP]
fact' = BL [ACC 1, LDC (I 1), Op Equ, SEL,
	BL [ACC 2],
	BL [ACC 3, TLTRC, NIL, ACC 1, ACC 2, Op Mul, CONS, LDC (I 1), ACC 1, Op Sub, CONS, TAP]]


t1 = [BL cl, CLOS, NIL, LDC (I 2),CONS, APP, BL cl, CLOS, NIL, LDC (I 2), CONS, APP, Op Add]
cl = [ACC 1, LDC (I 1), Op Add, RTN]

t2   = [fact, LTRC, NIL, LDC (I 1), CONS, LDC (I 5), CONS, APP]
fact = BL [ACC 1, LDC (I 1), Op Equ, SEL,
	BL [ACC 2,RTN],
	BL [ACC 3, LTRC, NIL, ACC 1, ACC 2, Op Mul, CONS, LDC (I 1), ACC 1, Op Sub, CONS, APP],RTN]




t4 = [revs, TLTRC, NIL, NIL, CONS, fibbd, TLTRC, NIL, LDC (I 2), CONS, NIL, LDC (I 1), CONS, LDC (I 1), CONS, CONS, APP, CONS, TAP]
fibbd = BL [ACC 2, LDC (I 0), Op Equ, SEL,
	BL [ACC 1, RTN],
	BL [ACC 3, TLTRC, NIL, LDC (I 1), ACC 2, Op Sub, CONS, ACC 1, ACC 1, CAR, ACC 1, CDR, CAR, Op Add, CONS, CONS, TAP]]


t5 = [revs, TLTRC, NIL, NIL, CONS, NIL, LDC (I 1), CONS, LDC (I 2), CONS, CONS, TAP]
revs = BL [ACC 1, NULL, SEL,
	BL [ACC 2],
	BL [ACC 3, TLTRC, NIL, ACC 2, ACC 1, CAR, CONS, CONS, ACC 1, CDR, CONS, TAP]]

t6 = [ fibe, TLTRC, NIL, revs, TLTRC, CONS, LDC (I 2), CONS, NIL, LDC (I 1), CONS, LDC (I 1), CONS, CONS, APP]
fibe = BL [ACC 2, LDC (I 0), Op Equ, SEL,
	BL [ACC 3, NIL, NIL, CONS, ACC 1, CONS, TAP],
	BL [ACC 4, TLTRC, NIL, ACC 3, CONS, LDC (I 1), ACC 2, Op Sub, CONS, ACC 1, ACC 1, CAR, ACC 1, CDR, CAR, Op Add, CONS, CONS, TAP]]

--Stack operations

pushS :: Code -> Secd ()
pushS v = do
	(s, e, c) <- get
	put (v:s, e, c)
	return ()

popS :: Secd ()
popS = do
	(s, e, c) <- get
	put (tail s, e, c)
	return ()

topS :: Secd Code
topS = do
	(s, e, c) <- get
	put (tail s, e, c)
	return $ head s

topC :: Secd Code
topC = do
	(s, e, c) <- get
	put (s, e, tail c)
	return $ head c

popE :: Secd ()
popE = do
	(s, e, c) <- get
	put (s, tail e, c)
	return ()



