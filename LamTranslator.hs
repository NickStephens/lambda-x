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

data Statement = Module String | Alias String Exp | Import String | Comment
	deriving Show

class Writeable a where
	write :: a -> String

instance Writeable Statement where
	write (Module name) = "module" ++ " " ++ name ++ " " ++ "where"
	write (Alias id exp) = id ++ " = " ++ (write exp)
	write (Import package) = "import" ++ " " ++ package
	write (Comment) = ""

instance Writeable Exp where
	write (Var v) = "(" ++ "Var" ++ " " ++ "\"" ++ v ++ "\"" ++ ")"
	write (Cons c) = c
	write (App e1 e2) = "(" ++ "App" ++ " " ++ (write e1) ++ " "
					++ (write e2) ++ ")" 
	write (Lam v e) = "(" ++ "Lam" ++ " " ++ "\"" ++ v ++ "\"" ++ " " ++ (write e) ++ ")"
{- faulty reduction ordering
	write (AReduce e) = "(AReduce" ++ " " ++ (write e) ++ ")"
	write (NReduce e) = "(NReduce" ++ " " ++ (write e) ++ ")"
-}

linebreak = try (string "\r\n") <|> string "\n"

-- Statement Parser
-- statement ::= alias | title | select | comment
statement = try (alias) <|> pmodule <|> pimport <|> comment

-- Module Parser
-- pmodule ::= 'module' letter+ '\n'
pmodule :: Parser Statement
pmodule = do
	string "module"
	many1 $ char ' ' <|> char '\t'
	ident <- many1 letter
	many $ char ' ' <|> char '\t'
	linebreak
	return $ Module ident

-- Alias Parser
-- alias ::= Letter+ '=' expression
alias :: Parser Statement 
alias = do
	ident <- many1 letter 
	many space
	char '='
	many space
	exp <- expression
	char ';'
	return $ Alias ident exp

-- Import Parser
-- pimport ::= 'import' letter+ '\n'
pimport :: Parser Statement
pimport = do
	string "import"
	many1 $ char ' ' <|> char '\t'
	ident <- many1 letter
	many $ char ' ' <|> char '\t'
	linebreak
	return $ Import ident

-- Comment Parser
-- comment ::= '-''-' (any text) '\n'
comment :: Parser Statement
comment = do
	string "--"
	many (noneOf "\n")
	linebreak
	return Comment

-- File Aggregation
-- chop the lambda expressions into distinct statements 
chop :: Parser [Statement]
chop = do{ a <- statement 
	 ; many space
	 ; do{ as <- chop
	     ; return (a:as)
	     }
	   <|> return [a]
	}

aggregate file = do
	contents <- readFile file
	return $ foldl (++) "" $ map ((++"\n") . write) $ run chop contents

translate file = do
	code <- aggregate file
	writeFile outputName code
	return $ "produced " ++ outputName
	where outputName = (takeWhile (/= '.') file) ++ ".lam.hs"
