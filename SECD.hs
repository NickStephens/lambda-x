module SECD where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

type Scratch = [Value]
type Env     = [[Value]]
type Code   = [Instr]
type Dump    = [Store]
data Store   = Br [Instr] | Call (Scratch, Env, Code) deriving Show

type Moment  = (Scratch, Env, Code, Dump)

data Value   = I Int | L [Value] | C Closure | B Bool | Cl Closure deriving (Eq,Show)
type Var     = String
type Closure = (Func, Env)
type Const   = Int
type Func    = [Instr]
data Oper    = Add | Sub | Mul | Div | Mod deriving (Show, Eq)
data Rela    = RLT | RGT | REQ deriving (Show, Eq)
data Instr =
			LDC Value | LD (Int, Int) | DUP |
			O Oper |
			R Rela |
			SEL | JOIN | Block [Instr] |
			NIL | CONS | CAR | CDR | ATOM |
			LDF Func | AP | RTN deriving (Eq,Show)

delta :: Secd ()
delta = do
	(s, e, c, d) <- get
	case head c of
		LDC x -> put (x:s, e, tail c, d)
		DUP -> do
			let x = head s
			put (x:s, e, c, d)
		O op -> do
			rslt <- oper op s
			put (rslt:((tail . tail) s), e, tail c, d) --pop two operands off S
		R rel -> do
			rslt <- rela rel s
			put (rslt:((tail . tail) s), e, tail c, d) --pop two operands off S
		SEL -> do
			let (Block ct:Block cf:rest) = tail c
			case head s of
				B True -> do
					put (s, e, ct, Br rest:d)
				B False -> do
					put (s, e, cf, Br rest:d)
				x   -> throwError ("expecting boolean value, found: " ++ (show x))
		JOIN -> do
			let Br jn = head d
			put (s, e, jn, tail d)
		NIL  -> do
			put (L []:s, e, tail c, d)
		CONS -> do
			let (a:L as:rest) = s
			put ((L (a:as)):rest, e, tail c, d)
		CAR -> do
			let L as = head s
			put (head as:tail s, e, tail c, d)
		CDR -> do
			let L as = head s
			put (L (tail as):tail s, e, tail c, d)
		ATOM -> do --isEmpty
			let a = head s
			if a==L []
				then put (B True:tail s, e, tail c, d)
				else put (B False:tail s, e, tail c, d)
		LDF f -> do
			let cl = (f, e)
			put (Cl cl:s, e, tail c, d)
		AP -> do
			let st = (s, e, tail c)         --stored state for the call
			let Cl (f, e) = head s     --the closure
			let L v  = head $ tail s   --arguments to f
			put ([], v:e, f, Call st:d)
		LD (i, j) -> do
			let iv = e !! (i-1)
			let jv = iv !! (j-1)
			put (jv:s, e, tail c, d)
		RTN -> do
			let x = head s
			let Call (s', e', c') = head d
			put (x:s', e', c', tail d)

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
	|rel == RLT = B $ i < i'
	|rel == RGT = B $i > i'
	|rel == REQ = B $ i == i'

--type Secd = ErrorT String (StateT Moment IO)
type Secd = StateT Moment (ErrorT String IO)

run p = runtest ([], [], p, [])

run' :: Secd ()
run' = do
	(s,e,c,d)  <- get
	liftIO $ print ("S: "  ++ (show s) ++ "   E: " ++ (show e))
	delta
	liftIO $ print ("Inst: " ++ (show$head c))
	secd' <- get
	case secd' of
		(v, e, [], []) -> do
			liftIO $ print ("result: "  ++ (show  v))
			return ()
		secd''         -> run'

--runtest tp = evalStateT (runErrorT run') tp
runtest tp = runErrorT (evalStateT run' tp)

t1 = [LDC (I 3), LDC (I 5), O Add, LDC (I 10), R RLT]
t2 = [NIL, LDC (I 1), CONS, LDC (I 2), CONS, CDR, CAR]
t3 = [NIL, LDC (I 3), CONS, LDF f, AP]
f  = [LD (1,1), LD (1,1), O Mul, RTN]
t4 = [NIL, sqr, CONS, LDF [NIL, LDC (I 3), CONS, LD (1,1), AP, RTN], AP]
sqr = LDF [LD (1,1), LD (1,1), O Mul, RTN]

--Stack operations

pushS :: Value -> Secd ()
pushS v = do
	(s, e, c, d) <- get
	put (v:s, e, c, d)
	return ()

popS :: Secd ()
popS = do
	(s, e, c, d) <- get
	put (tail s, e, c, d)
	return ()

topS :: Secd Value
topS = do
	(s, e, c, d) <- get
	put (tail s, e, c, d)
	return $ head s





