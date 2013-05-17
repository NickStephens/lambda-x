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
data Oper    = Add | Sub | Mul | Div | Mod | Not | Neg | Lt | Gt | Equ | Or | And | Cdr | Car | Cons | Fst | Snd | Neq
		deriving (Show, Eq, Ord)
data Code =
			ACC Int |
			CLOS | LETREC | TLTRC | STOS Block |
			LET |
			ENDLET |
			SEL |
			BL Block | --load func
			RC Block |
			APP | TAP | RAP |
			RTN |
			LDC Code |
			OP Oper |
			NIL | CONS | CAR | CDR | NULL | FS | SN |
			I Integer | D Double | L [Code] | CL Closure | B Bool | P (Code,Code) |
			E Env | C Char |
			PATTERN_ERR
				deriving (Eq)

instance Show Code where
	show (I i) = show i --"(I "++show i++")"
	show (L v) = show v --"(L "++show v++")"
	show (P (c1, c2)) = show (c1, c2)
	show (CL (f, e)) = "CL {{ "++show f++"  ||  "++show e++ " }}" -- ++show e
	show (B b) = show b
	show (E env) = "E " ++show env
	show (BL block) = "BL "++show block
	show CLOS = "CLOS"
	show (ACC i) = "ACC " ++show i
	show LET = "LET"
	show ENDLET = "ENDLET"
	show SEL = "SEL"
	show APP = "APP"
	show RAP = "RAP"
	show RTN= "RTN"
	show (LDC c) = "LDC " ++ show c
	show (OP o) = "OP " ++ show o
	show NIL = "NIL"
	show CONS = "CONS"
	show CAR = "CAR"
	show CDR = "CDR"
	show TAP = "TAP"
	show FS = "FST"
	show SN = "SND"
	show (RC b) = "RC " ++ show b
	show LETREC = "LETREC"
	show PATTERN_ERR = "PATTERN_ERR"


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
		LETREC -> do
			let (BL bl) = head s
			let (BL bl') = head bl
			put (CL (bl',BL bl:e):tail s, e, c)
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
	 		let (v:CL (c',e'):rest) = s
			put (BL c:E e:rest, v:e', c')
		RTN -> do
			let (v:BL c':E e':rest) = s
			put (v:rest, e', c')
		OP op -> do
			rslt <- oper op s
			put (rslt:((tail . tail) s), e, c) --pop two operands off S
		FS -> do
			let (P (a,b):rest) = s
			put (a:rest, e, c)
		SN -> do
			let (P (a,b):rest) = s
			put (b:rest, e, c)
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
		TAP -> do
	 		let (v:CL (c',e'):rest) = s
			put (rest, v:e', c')
		PATTERN_ERR -> do
			throwError "(!) Pattern Match Failure"



rplaca v = case v of
	CL (f,e) -> CL (f,(rplaca v):e)


oper :: Oper -> Scratch -> Secd Code
oper op s
	|length s < 2 = throwError "not enough values on stack to operate on"
	|otherwise = case head s of
		I i -> case head (tail s) of
			I i' -> return (appI op i i')
			_    -> throwError "second arg not an int"
		B b  -> case head (tail s) of
			B b' -> return (appB op b b')
			_    -> throwError "second arg not a boolean"
		L l -> case head (tail s) of
			L l' -> return (appL op l l')
		P p@(p1, p2) -> case head (tail s) of
			P p' -> return (appP op p p')

appI op i i'
	|op == Add = I $ i + i'
	|op == Sub = I $ i - i'
	|op == Mul = I $ i * i'
	|op == Div = I $ i `div` i'
	|op == Mod = I $ i `mod` i'
	|op == Lt = B $ i < i'
	|op == Gt = B $i > i'
	|op == Equ = B $ i == i'

appB op b b'
	|op == Or = B $ b || b'
	|op == And = B $ b && b'

appL op l l'
	|op == Equ = B $ l==l'
	|op == Neq = B $ l/=l'

appP op p p'
	| op == Equ = B $ p==p'

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
--	liftIO $ putStrLn ("S: " ++ (show s))
--	liftIO $ putStrLn ("E: " ++ (show e))
--	liftIO $ putStrLn ("C: " ++ (show c))
	delta
--	liftIO $ putStrLn ""
--	liftIO $ putStrLn ("Code: " ++ (show$head c)++" ->")
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



