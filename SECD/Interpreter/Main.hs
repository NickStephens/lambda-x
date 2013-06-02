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

initializeInterpreter = do
			mods <- load "pcons/prelude.pcons"
			runInterpreter mods

runInterpreter mods = do
		inp <- readline "> "
		case inp of
			Nothing -> return ()
			Just cmd -> do
				let command = parseCommand cmd
				case command of
					QuitCmd -> return ()
					LoadCmd filename -> do 
							ld <- load filename
							runInterpreter $ mods ++ ld
					LetCmd alias -> do
							runInterpreter $ mods ++ [alias]
					ShowCmd -> do
							putStr $ (showSymbols mods) ++ "\n"
							runInterpreter mods
					ExpressionCmd expr -> do
							processAndRun expr mods
							runInterpreter mods
					_ -> runInterpreter mods

{- SHOW SYMBOLS -}

-- shows all available symbols loaded into the environment

showSymbols mods = case mods of
			[mod] -> extractName mod
			(m:ms) -> (extractName m) ++ ", " ++ showSymbols ms 
	where 
		extractName alias = case alias of
			NoRec name prms expr -> name 
			Recr name prms expr -> name 
			TRec name prms expr -> name


{- PROCESS AND RUN -}

-- runs a parsed expression in the interpreter

processAndRun input mods = evalStateT (processAndRun' input mods) ([Map.empty], 1)


processAndRun' input mods = do
		state <- funcStream $ desugar $ mods ++ [(NoRec "tmp" [] input)]
		e <- comp state
		liftIO $ run e
		return ()

{- LOAD -}

-- loads a module into the interactive environment

load filename = do
		contents <- parseFile filename
		case contents of
			Right res -> return res
