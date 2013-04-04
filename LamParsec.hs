module LamParsec where

import Text.ParserCombinators.Parsec
import Lambda

--run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
				Left err -> error "ykes(!)"
				Right x -> x

expression =	chainl1 (variable <|> abstraction <|> parexp) comb <|>
				variable <|>
				abstraction

parexp = do
	string "("
	e <- expression
	string ")"
	return e

comb = do
	space
	return App

abstraction :: Parser Exp
abstraction = do
		string "L"
		(Var v) <- variable
		string "."
		e <- expression
		return (Lam v e)

variable :: Parser Exp
variable = do
		a <- lower
		return (Var [a])

