module SECD where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

type Scratch = [Value]
type Env     = [[Value]]
type Code   = [Instr]
type Dump    = [Store]
data Store   = BRT [Instr] | Call (Scratch, Env, Code) deriving Show

type Moment  = (Scratch, Env, Code, Dump)

data Value   = I Integer | L [Value] | Cl Closure | B Bool deriving (Eq)
type Var     = String
type Closure = (Func, Env)
type Const   = Integer
type Func    = [Instr]
data Oper    = Add | Sub | Mul | Div | Mod deriving (Show, Eq)
data Rela    = RLT | RGT | REQ deriving (Show, Eq)
data Instr =
			LDC Value | LD (Int, Int) | DUP |
			O Oper |
			R Rela |
			SEL | JOIN | Block [Instr] |
			NIL | CONS | CAR | CDR | NULL |
			LDF Func | AP | RTN | DUM | RAP deriving (Eq,Show)

instance Show Value where
	show (I i) = show i
	show (L v) = show v
	show (Cl (f, e)) = "Cl ("++show f++",env)"
	show (B b) = show b

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
					put (tail s, e, ct, BRT rest:d)
				B False -> do
					put (tail s, e, cf, BRT rest:d)
				x   -> throwError ("expecting boolean value, found: " ++ (show x))
		JOIN -> do
			let BRT jn = head d
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
		NULL -> do
			let a = head s
			if a==L []
				then put (B True:tail s, e, tail c, d)
				else put (B False:tail s, e, tail c, d)
		LDF f -> do
			let cl = (f, e)
			put (Cl cl:s, e, tail c, d)
		AP -> do
			let str = ((tail.tail) s, e, tail c)    --stored state for the call
			let Cl (f, e') = head s     --the closure
			let L v  = head $ tail s    --arguments to f
			put ([], v:e', f, Call str:d)
		LD (i, j) -> do
			let iv = e !! (i-1)
			let jv = iv !! (j-1)
			put (jv:s, e, tail c, d)
		RTN -> do
			let x = head s
			let Call (s', e', c') = head d
			put (x:s', e', c', tail d)
		DUM -> do
			put (s, []:e, tail c, d)
		RAP -> do
			let Cl (f,n:e') = head s
			let L (v:x) = head . tail $ s
			put ([], (rplaca v):e', f, Call ((tail.tail) s, tail e, tail c):d)

rplaca v = case v of
	Cl (f,n:e) -> [Cl (f,(rplaca v):e)]


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
	liftIO $ putStrLn ("S: " ++ (show s))
	liftIO $ putStrLn "E:"
	displayEnv e
	liftIO $ putStrLn ("C: " ++ (show c))
	if null d then (liftIO $ putStrLn ("D: []")) else (liftIO $ putStrLn ("D: " ++ (show (head d))))
	delta
	liftIO $ putStrLn ""
	liftIO $ putStrLn ("Inst: " ++ (show$head c)++" ->")
	secd' <- get
	case secd' of
		(v, e, [], []) -> do
			liftIO $ putStrLn ("result: "  ++ (show  v))
		secd''         -> run'

displayEnv :: Env -> Secd ()
displayEnv e = do
	if null e
		then do
			liftIO $ putStrLn "   []"
			return ()
		else do
			liftIO $ putStrLn ("   "++(show.head) e)
			displayEnv $tail e
--runtest tp = evalStateT (runErrorT run') tp
--runtest tp = runErrorT (evalStateT run' tp)

t1 = [LDC (I 3), LDC (I 5), O Add, LDC (I 10), R RLT]
t2 = [NIL, LDC (I 1), CONS, LDC (I 2), CONS, CDR, CAR]

t3 = [NIL, LDC (I 3), CONS, LDF f, AP]
f  = [LD (1,1), LD (1,1), O Mul, RTN]

t4 = [NIL, sqr, CONS, LDF [NIL, LDC (I 3), CONS, LD (1,1), AP, RTN], AP]
sqr = LDF [LD (1,1), LD (1,1), O Mul, RTN]

t5 = [DUM, NIL, ltrec, CONS, cont, RAP]
ltrec = LDF [LD (1,1), NULL, SEL, thenBl, elseBl, RTN]
thenBl = Block [LD (1,2), JOIN]
elseBl =   Block [NIL, LDC (I 1), LD (1,2), O Add, CONS, LD (1,1), CDR, CONS, LD (2,1), AP, JOIN]
cont = LDF [NIL, LDC (I 0), CONS, LDC (L [(I 1),(I 2),(I 3)]), CONS, LD (1,1), AP, RTN]

t6 = [NIL, LDC (I 1), CONS, LDC (I 3), CONS, ldf, AP]
ldf = LDF [DUM, NIL, ldf', CONS, LDF [NIL, LD (2,2), CONS, LD (2,1), CONS, LD (1,1), AP, RTN], RAP, RTN]
ldf' = LDF [LDC (I 0), LD (1,1), R REQ, SEL, Block [LD (1,2), JOIN], Block [NIL, LD (1,2), LD (1,1), O Mul, CONS, LD (3,2), LD (1,1), O Sub, CONS, LD (2,1), AP, JOIN], RTN]


--other guys secd fact (broke)
code1 = [DUM, NIL]
code2 = [LDF [LD (0,0), LDC (I 0), R REQ, SEL, Block [LDC (I 1), JOIN], Block [NIL, LD (0,0), LDC (I 1), O Sub, CONS, LD (1,0), AP, LD (0,0), O Mul, JOIN], RTN], CONS]
code3 = [LDF [NIL, LDC (I 6), CONS, LD (1,0), AP, RTN]]
code4 = [RAP]
codeF = code1 ++ code2 ++ code3 ++ code4





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






