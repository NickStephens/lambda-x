module Interpreter.CmdAbstractSyntax where

import AbstractSyntax

data Cmd = LetCmd Alias | LoadCmd String | ExpressionCmd Expr | QuitCmd | ShowCmd
	deriving (Show, Eq)
