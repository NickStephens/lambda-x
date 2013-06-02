module Interpreter.Main where

import Text.ParserCombinators.Parsec
import Parser
import AbstractSyntax
import Interpreter.CmdParser
import Interpreter.CmdAbstractSyntax
import Desugarer (desugar, expressionDesugar)
import Translator (funcStream)
import Compiler (comp)
import SEC (run)
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import System.Console.Readline

--runInterpreter :: StateT (ErrorT String IO ())

-- LoadedModules
-- LoadedModule Code
-- History Stack
{-
data InterpreterState = IState [Module] Program [String]

type Module = String
-}

{-
intializeInterpreter = do
			mods <- load "Prelude.txt"
			runInterpreter mods
-}

runInterpreter = do
		inp <- readline "> "
		case (inp) of
			Nothing -> return ()
			Just cmd -> do
					-- mods <- getModules
					mods <- load "pcons/prelude.pcons"
					-- place expr in history
					-- lookup names and substitute
					processAndRun cmd mods
					runInterpreter


parsepecans input = case (parse expression "" input) of
		Right res -> putStr $ (++) (show res) "\n"

--hacky solution: always prepend the prelude to process and run
--when loading scripts, prepend those also this

-- processAndRun should be passed a lookup table

processAndRun input mods = evalStateT (processAndRun' input mods) ([Map.empty], 1)


processAndRun' input mods = do
		let pres = case (parse expression "" input) of
				Right res -> res
		state <- funcStream $ desugar $ mods ++ [(NoRec "tmp" [] pres)]
		e <- comp state
		liftIO $ run e
		return ()

load filename = do
		contents <- parseFile filename
		case contents of
			Right res -> return res
