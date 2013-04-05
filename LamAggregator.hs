module LamAggregator where

import Lambda
import LamParsec
import Text.ParserCombinators.Parsec
import Data.Char

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

class Writeable a where
	write :: a -> String

instance Writeable Alias where
	write (Alias id exp) = id ++ " = " ++ (write exp)

instance Writeable Exp where
	write (Var v) = "(" ++ "Var" ++ " " ++ v ++ ")"
	write (Cons c) = c
	write (App e1 e2) = "(" ++ "App" ++ " " ++ (write e1) ++ " "
					++ (write e2) ++ ")" 
	write (Lam v e) = "(" ++ "Lam" ++ " " ++ v ++ " " ++ (write e) ++ ")"

-- Alias Parser
-- alias ::= Letter+ '=' expression
alias :: Parser Alias
alias = do
	ident <- many1 letter 
	many space
	char '='
	many space
	exp <- expression
	char ';'
	return $ Alias ident exp

-- File Aggregation
-- chop the lambda expressions into distinct aliases
chop :: Parser [Alias]
chop = do{ a <- alias
	 ; many space
	 ; do{ as <- chop
	     ; return (a:as)
	     }
	   <|> return [a]
	}

aggregate file = do
	contents <- readFile file
	return $ foldl (++) "" $ map ((++"\n") . write) $ run chop contents
	where output = takeWhile (/= '.') file

-- foldl (++) "" (map ((++"\n") . write) lambda)

lambda = [Alias "zero" (Lam "f" (Lam "x" (Var "x"))), Alias "two" (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x")))))] 
