module Parser where 

import Prelude hiding (LT, GT, EQ, div)
import qualified Data.Map as Map
import AbstractSyntax 
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language ( haskellDef )

lexer = T.makeTokenParser (haskellDef)

identifier = T.identifier lexer
lexeme = T.identifier lexer
whiteSpace = T.whiteSpace lexer
symbol = T.symbol lexer
natural = T.natural lexer
semi = T.semi lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer


{- BINARY SUGAR -}

binary = do
	many space
	op <- operator 
	many space
	return (\e1 -> \e2 -> (App (App op e1) e2))

{- EXPRESSION -}
expression :: Parser Expr
expression = chainl1 term binary

term = (chainl1 (variable <|> lambda 
			<|> operator <|> parexpression) application)


{- PARENTHESIZED EXPRESSION -}
parexpression :: Parser Expr
parexpression = do
	between (symbol "(") (symbol ")") expression

{- APPLICATION -}
application = do
	many1 space
	return App

{- LAMBDA ABSTRACTION -}
lambda = do
	symbol "\\"
	name <- identifier
	symbol "."
	test <- expression 
	return $ Lam name test

{- VARIABLE -}

variable = do
	head <- letter
	tail <- many alphaNum	
	return $ Var (head:tail)

{- OPERATORS -}

operator = do
	try (elt) <|> try (egt) <|> lt <|> gt <|> eq <|> try (neq)
	<|> add <|> sub <|> mul <|> div
	<|> try (car) <|> try (cdr) <|> try (cons) <?> "operator"

-- ARITHMENTIC OPERATORS
add :: Parser Expr
add = do
	char '+'
	return $ Op ADD


sub :: GenParser Char st (Expr)
sub = do
	char '-'
	return $ Op SUB


mul :: GenParser Char st (Expr)
mul = do
	char '*'
	return $ Op MUL


div :: GenParser Char st (Expr)
div = do
	char '/'
	return $ Op DIV 

-- RELATIONAL OPERATORS
lt :: GenParser Char st (Expr)
lt = do
	char '<'
	return $ Op LT


gt :: GenParser Char st (Expr)
gt = do
	char '>'
	return $ Op GT 

elt :: GenParser Char st (Expr)
elt = do
	string "<="
	return $ Op ELT

egt :: GenParser Char st (Expr)
egt = do
	string ">="
	return $ Op EGT

eq :: GenParser Char st (Expr)
eq = do
	string "=="
	return $ Op EQ

neq :: GenParser Char st (Expr)
neq = do
	string "/="
	return $ Op NEQ

-- LIST OPERATORS
car :: GenParser Char st (Expr)
car = do
	string "car"
	return $ Op CAR


cdr :: GenParser Char st (Expr)
cdr = do
	string "cdr"
	return $ Op CDR


cons :: GenParser Char st (Expr)
cons = do
	string "cons"
	return $ Op CONS 

{- INTERMEDIATE PARSING -}
