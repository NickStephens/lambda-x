module LamParsec where

import Text.ParserCombinators.Parsec
import Lambda

lparse :: String -> Exp 
lparse = run expression

--run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
				Left err -> error "ykes(!): "
				Right x -> x

-- Expression
-- expression ::= expression expression | abstraction | variable
expression = chainl1 (variable <|> abstraction <|> parexp) comb <|>
							   variable <|>
							   abstraction 

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

-- variable ::= lowercaseCharacter
variable :: Parser Exp
variable = do
		a <- lower
		return (Var [a])

