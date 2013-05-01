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
parens = T.parens lexer

{- PROGRAM -}

program :: Parser Program
program = alias `endBy` semi

{- ALIAS -}
alias :: Parser Alias
alias = do
	als <- name
	many1 space
	params <- name `sepEndBy` (char ' ')
	many space
	symbol "="
	exp <- expression
	return $ Alias als params exp

{- EXPRESSION -}
-- binary sugar and operator application has higher precedence 
-- then functional application

expression = list    `chainl1` (binary relop)
list 	   = factor  `chainl1` (binary cons)
term 	   = factor  `chainl1` (binary addop)
factor	   = app     `chainl1` (binary mulop)
app 	   = primary `chainl1` application
primary    = try (parexpression) <|> operator <|>  variable <|> lambda 

{- PARENTHESIZED EXPRESSION -}
parexpression = do
		char '('
		many space
		e <- expression
		many space
		char ')'
		return e

{- BINARY SUGAR -}
binary f = do
	op <- f
	return (\x -> \y -> (App (App op x) y))

{- APPLICATION -}
application = do
	char ' '
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
	try (elt) <|> try (egt) <|> lt <|> gt <|> eq <|> neq
	<?> "relational operator"

listop = do
	car <|> cdr <?> "list operator"

addop = do
	add <|> sub <?> "addition operator"

mulop = do
	mul <|> div <?> "multiplicative operator"

-- ARITHMENTIC OPERATORS
add = do
	char '+'
	return $ Op ADD


sub = do
	char '-'
	return $ Op SUB


mul = do
	char '*'
	return $ Op MUL


div = do
	char '/'
	return $ Op DIV 

-- RELATIONAL OPERATORS

lt = do
	char '<'
	return $ Op LT


gt = do
	char '>'
	return $ Op GT 


elt = do
	string "<="
	return $ Op ELT


egt = do
	string ">="
	return $ Op EGT


eq = do
	string "=="
	return $ Op EQ


neq = do
	string "/="
	return $ Op NEQ

notop :: Parser Expr
notop = do
	string "!"
	return $ Op NOT

-- LIST OPERATORS

car = do
	char '^'
	return $ Op CAR


cdr = do
	char '~'
	return $ Op CDR


cons = do
	char ':'
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
