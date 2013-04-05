module LamTranslator (translate) where

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

class Writeable a where
	write :: a -> String

instance Writeable Alias where
	write (Alias id exp) = id ++ " = " ++ (write exp)

instance Writeable Exp where
	write (Var v) = "(" ++ "Var" ++ " " ++ "\"" ++ v ++ "\"" ++ ")"
	write (Cons c) = c
	write (App e1 e2) = "(" ++ "App" ++ " " ++ (write e1) ++ " "
					++ (write e2) ++ ")" 
	write (Lam v e) = "(" ++ "Lam" ++ " " ++ "\"" ++ v ++ "\"" ++ " " ++ (write e) ++ ")"

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

lambdaPrelude = "import Lambda\n"

aggregate file = do
	contents <- readFile file
	return $ (lambdaPrelude ++) $ foldl (++) "" $ map ((++"\n") . write) $ run chop contents

translate file = do
	code <- aggregate file
	writeFile outputName code
	return $ "produced " ++ outputName
	where outputName = (takeWhile (/= '.') file) ++ ".hs"
