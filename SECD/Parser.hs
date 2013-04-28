module Parser where

import Text.ParserCombinators.Parsec

{-
script :: GenParser Char s Script
script = do
		many1 def
-}

{- 
defwrap :: GenParser Char s (String, [String], Expr)
defwrap = do
	name <- identifier
	many1 space
	params <- sepBy arg (char (many1 space))
	char '='
	many1 space
	exp <- expression
	return $ Def name params expression
-}

{-
letrec :: GenParser Char s Def
letrec = do
	string "Letrec"
	(name, params, expression) <- defwrap
	return $ Def name (Rec params expression)
-}

{-
tletrec = do
	string "Tletrec"
	(name, params, expr) <- defwrap
	return $ Def name (TRec params expr)
-}

{-
expression = <|> <?> "expression"
-}

{-
lambda = do
	char 'L'
	many space
	name <- identifier
	many space 
	char '.'
	many space
	expr <- expression		
	return $ Lam name expr
-}

var = do
	name <- identifier
	return $ Var name

App
