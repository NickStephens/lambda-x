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

main = initializeInterpreter

{- INITIALIZE INTERPRETER -}

-- loads the prelude into the interpreter and starts it

initializeInterpreter = do
			prettyHeaderPrinter
			putStrLn "coauthored by Owen Meyer and Nick Stephens (2013)"
			let prelude = "./pcons/prelude.pcons"
			putStrLn "loading opii (Olympia PCONS Interpreter interactive) ..."
			putStrLn $ "loading prelude from ... " ++ prelude
			mods <- load prelude
			runInterpreter mods

{- RUN INTERPRETER -}

-- the heart of the interpreter

runInterpreter mods = do
		inp <- readline "opii (!) > "
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
							putStrLn (showSymbols mods)
							runInterpreter mods
					ExpressionCmd expr -> do
							processAndRun expr mods
							runInterpreter mods

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

{- PRETTY HEADER PRINTER -}

-- prints the OPIi header in all of its glory

prettyHeaderPrinter = do
			putStrLn "           ________     _________   ____"
			putStrLn "          /       /    /        /    /      *"
			putStrLn "         /       /    /        /    /"
			putStrLn "        /       /    /--------     /      /"
			putStrLn "       /       /    /             /      /"
			putStrLn "      /_______/    /           __/__    /"
			putStrLn "    -----------------------------------------"


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
