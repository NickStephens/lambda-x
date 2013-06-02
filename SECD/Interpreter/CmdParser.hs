module Interpreter.CmdParser where

import Text.ParserCombinators.Parsec
import Interpreter.CmdAbstractSyntax
import Parser (parseFile, alias, expression)

commandParse input = parse command "" input

-- command := !load <filename> | !quit | !let <alias_definiton> | <expression>
command = try letcmd <|> try loadcmd <|> try quitcmd <|> expressioncmd


loadcmd = do
	string "!load"
	many1 space
	fname <- filename
	return $ LoadCmd fname	

letcmd  = do 
	string "!let"
	many1 space
	tempalias <- alias
	return $ LetCmd tempalias	

quitcmd = do
	string "!quit"
	return QuitCmd

expressioncmd = do
	tempexpr <- expression
	return $ ExpressionCmd tempexpr

filename = do
	head <- letter
	tail <- many (alphaNum <|> char '.')
	return (head:tail)
