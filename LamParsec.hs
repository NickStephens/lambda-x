module LamParsec where

import Text.ParserCombinators.Parsec
import Lambda

lparse :: String -> Exp 
lparse = run expression

--run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
				Left err -> error "need better error handler(!): "
				Right x -> x

-- Expression
-- expression ::= expression expression | variable | abstraction | name
expression = chainl1 (variable <|> abstraction <|> parexp <|> name) comb <|>
							   variable <|>
							   abstraction <|> name

-- Parenthesized Expression
-- parexp ::= '(' expression ')'
parexp = do
	string "("
	many space
	e <- expression
	many space
	string ")"
	return e

-- comb ::= ' '
comb = do
	space  -- operator
	return App

-- abstraction ::= 'L' lower '.' expression

abstraction :: Parser Exp
abstraction = do
		string "L"
		many space
		(Var v) <- variable
		many space
		string "."
		many space
		e <- expression
		return (Lam v e)

-- variable ::= lowercaseString
variable :: Parser Exp
variable = do
		a <- many1 lower
		return (Var a)

-- constant ::= #lowercaseString
name :: Parser Exp
name = do
	string "#"
	nm <- many1 letter
	return (Cons nm)

