

module LamParsec where

import Text.ParserCombinators.Parsec
import Lambda

--run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
				Left err -> error "ykes(!)"
				Right x -> x



--expression :: Parser Exp
expression = between (string "(") (string ")")
		(chainl (variable <|> abstraction) comb (Var "x") )
		<|>
		(chainl (variable <|> abstraction) comb (Var "x") )


comb = do
	many1 space
	return App

combination :: Parser Exp
combination = do
		a <- expression
		space
		b <- expression
		return (App a b)

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






