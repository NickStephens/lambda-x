module Interpreter where

import Parser (expression)
import AbstractSyntax
import Text.ParserCombinators.Parsec (parse)
import Desugarer (desugar, expressionDesugar)
import Translator (funcStream)
import Compiler (comp)
import SEC (run)
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import System.Console.Readline

--runInterpreter :: StateT (ErrorT String IO ())

runInterpreter = do
		putStr "> "
		inp <- getLine
		processAndRun inp
		runInterpreter

processAndRun input = evalStateT (processAndRun' input) ([Map.empty], 1)


processAndRun' :: String -> ErrorT StateT ([Compiler.Scope], Int) IO PCONS.EXP
processAndRun' input = do
		let pres = case (parse expression "" input) of
				Right res -> res
		state <- funcStream $ desugar [(NoRec "tmp" [] pres)]
		e <- comp state
		liftIO $ run e
		return ()
