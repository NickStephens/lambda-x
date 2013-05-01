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

{- ALIAS -}
alias :: Parser Alias
alias = do
	als <- name
	many1 space
	params <- name `sepEndBy` (char ' ')
	symbol "="
	exp <- expression
	semi
	return $ Alias als params exp

{- BINARY SUGAR -}
binary = do
	many space
	op <- operator 
	many space
	return (\e1 -> \e2 -> (App (App op e1) e2))

{- EXPRESSION -}
-- binary sugar and operator application has higher precedence


expression :: Parser Expr
expression = term `chainl1` application

factor = chainl1 (variable <|> lambda 
			<|> operator <|> parexpression) application

term = (chainl1 (variable <|> lambda 
			<|> operator <|> parexpression) (binary)) <|> factor  


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
	nm <- name
	return $ Var nm

{- OPERATORS -}

operator = do
	relop <|> addop <|> mulop <|> listop <?> "operator"


relop = do
	try (elt) <|> try (egt) <|> lt <|> gt <|> eq <|> try (neq)
	<?> "relational operator"

listop = do
	try(cons) <|> try (car) <|> cdr <?> "list operator"

addop = do
	add <|> sub <?> "addition operator"

mulop = do
	mul <|> div <?> "multiplicative operator"

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

{- LET -}

{-
letinp :: Parser Expr
letinp = do
	reserved "let"
	als <- alias
	many1 space
	reserved "in"
	inexp <- expression	
	return Let als inexp	
-}

{- CASE -}

{- VALUES -}

{- INTERMEDIATE PARSING -}
name :: Parser Name
name = do
	head <- letter
	tail <- many alphaNum
	return (head:tail)

eol = do { try (string "\n\r") <|> try (string "\r\n") 
	  <|> string "\r" <|> string "\n"
	 ; return ()
	 }

end = do 
	eol <|> (many space >> eof)
