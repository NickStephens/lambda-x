module Parser where 

import Prelude hiding (LT, GT, EQ, div)
import AbstractSyntax 
import Text.ParserCombinators.Parsec

{-
script :: GenParser Char s Script
script = do
		many1 def
-}

{- 
defwrap :: GenParser Char s (String, [String], Expr)
defwrap = do
	name <- identifier
	many1 space
	params <- sepBy arg (char (many1 space))
	char '='
	many1 space
	exp <- expression
	return $ Def name params expression
-}

{-
letrec :: GenParser Char s Def
letrec = do
	string "Letrec"
	(name, params, expression) <- defwrap
	return $ Def name (Rec params expression)
-}

{-
tletrec = do
	string "Tletrec"
	(name, params, expr) <- defwrap
	return $ Def name (TRec params expr)
-}

{-
expression = <|> <?> "expression"
-}

{-
lambda = do
	char 'L'
	many space
	name <- identifier
	many space 
	char '.'
	many space
	expr <- expression		
	return $ Lam name expr
-}

{-
var = do
	name <- identifier
	return $ Var name
-}

{- OPERATORS -}

operator = do
	try (elt) <|> try (egt) <|> lt <|> gt <|> eq <|> try (neq)
	<|> add <|> sub <|> mul <|> div
	<|> try (car) <|> try (cdr) <|> try (cons) <?> "operator"

-- ARITHMENTIC OPERATORS
add :: GenParser Char st (Expr)
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
