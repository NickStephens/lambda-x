module LamAggregator where

import Lambda
import LamParsec
import Text.ParserCombinators.Parsec

-- take a .lam file as input
-- and produce a haskell file containing the Abstract Syntax tree 
-- of the lambda expression tied to aliases (names)
-- resources:
--	Real World Haskell

-- recognize identifiers then equal signs
--	alias ::= identifier '=' expression
--	where expression is defined in LamParsec

data Alias = Alias String Exp
	deriving Show

term :: Parser Alias
term = do
	ident <- many1 letter 
	many space
	char '='
	many space
	exp <- expression
	return $ Alias ident exp
