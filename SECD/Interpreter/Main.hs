module Interpreter.Main where

import Control.Exception
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
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
			putStrLn ""
			putStrLn "coauthored by Owen Meyer and Nick Stephens (2013)"
			let prelude = "./pcons/prelude.pcons"
			putStr $ "loading prelude from " ++ prelude ++ " ... "
			mods <- load prelude
			case mods of 
				Left error -> do 
					putStrLn "failed"	
					disectError error
					putStrLn "WARNING: no base environment loaded (!)"
					putStrLn "Unless you're a lambda cowboy, prepare for a wild ride ..."
					runInterpreter []	
				Right pred -> do
					putStrLn "loaded"
					runInterpreter pred

{- RUN INTERPRETER -}

-- the heart of the interpreter

runInterpreter mods = do
		inp <- readline "opii (!) > "
		case inp of
			Nothing -> return ()
			Just "" -> runInterpreter mods  -- special empty input case
			Just cmd -> do
				let command = parseCommand cmd
				case command of
					Right cmd -> case cmd of 
						QuitCmd -> return ()
						LoadCmd filename -> do 
								res <- load filename
								case res of
									Left error -> do
										putStrLn $ "failed to load " ++ filename
										runInterpreter mods
									Right ld -> do 
										runInterpreter $ mods ++ ld
						LetCmd alias -> do
								runInterpreter $ mods ++ [alias]
						ShowCmd -> do
								putStrLn (showSymbols mods)
								runInterpreter mods
						ExpressionCmd expr -> do
								processAndRun expr mods
								runInterpreter mods
					Left err -> do 
						disectError err
						runInterpreter mods
							

{- DISPLAY MESSAGES -}

disectError err = do
		let pos = errorPos err
		let col = sourceColumn pos
		let row = sourceLine pos
		putStr $ "parse error at " ++ (show row) ++ ":" ++ (show col)
		putStrLn $ " " ++ (findUnexpected $ errorMessages err)
		where findUnexpected [] = ""
		      findUnexpected (e:es) = case e of
					SysUnExpect msg -> "(" ++ msg ++ ")"
					UnExpect msg -> "Unexpect " ++ msg
					_ -> findUnexpected es

{- SHOW SYMBOLS -}

-- shows all available symbols loaded into the environment

showSymbols mods = case mods of
			[] -> ""
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
			putStrLn "      Olympia PCONS Interpreter interactive"


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

load filename = (parseFile filename) --`catch` loadHandler

loadHandler :: Exception e => e -> IO (Either ParseError Program)
loadHandler _ = do
		putStrLn "failed to load"
		return $ Right []
