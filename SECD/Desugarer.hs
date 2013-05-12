module Desugarer where

import Parser 
import AbstractSyntax
import Text.ParserCombinators.Parsec (parse)

debug expr = case (parse expression "" expr) of
	Right res -> caseofDesugar res

{- CASEOF DESUGARING -}

-- any pattern matching on structures needs to account
-- for values existing within those structures
caseofDesugar :: Expr -> Expr
caseofDesugar (Case subject []) = Val (ValInt (-1)) --temp solution
caseofDesugar (Case subject (b:branches)) = case (fst b) of
	ValPattern val -> (COND (App (App (Op EQo) subject) val)
				  (snd b)
				  (caseofDesugar (Case subject branches)))
	List (x, xs) -> (COND 
			( valMatch subject (fst b)
				(App (App (Op NEQ) subject) (Lst []))
			)
		        (toLets subject (fst b) (snd b))
		        (caseofDesugar (Case subject branches)))
	Pair (x, y) -> 	(COND
		      	( valMatch subject (fst b) (Val (ValBool True)))
			(toLets subject (fst b) (snd b)) -- letOf
			(caseofDesugar (Case subject branches)))
	Symbol sym ->   (toLets subject (fst b) (snd b))
			


-- takes care of any values within structures
-- by appending the original boolean expression
-- with more boolean expressions serperated by
-- the logical AND operator
valMatch :: Expr -> Pattern -> Expr -> Expr
valMatch var pat expr = case pat of
	List (x,xs) -> case x of
			Symbol head -> expr
			ValPattern val -> appendAnd (App (Op CARo) var) val
				(valMatch (App (Op CDRo) var) xs expr) 
			struct -> valMatch (App (Op CARo) var) struct 
				(valMatch (App (Op CDRo) var) xs expr) 
	Pair (x,y) -> case x of
			Symbol fst -> expr
			ValPattern val -> appendAnd (App (Op FST) var) val 
				(valMatch (App (Op SND) var) y expr)
			struct -> valMatch (App (Op FST) var) struct
				(valMatch (App (Op SND) var) y expr)
	ValPattern val -> appendAnd var val expr
	Symbol _ -> expr

-- appends a logical AND to an expression
-- and then an expression of equality of 
-- var against the value val
appendAnd :: Expr -> Expr -> Expr -> Expr
appendAnd var val expr = App (App (Op AND) (App (App (Op EQo) var) val)) expr

-- toLets
-- translates a pattern with symbols or values
-- to let statements
toLets :: Expr -> Pattern -> Expr -> Expr
toLets var pat expr = case pat of
	List (x, xs) -> case x of
		Symbol sym -> prependLet sym (App (Op CARo) var) 
				(toLets (App (Op CDRo) var) xs expr)
		ValPattern val -> (toLets (App (Op CDRo) var) xs expr)  
		struct -> toLets (App (Op CARo) var) x 
				(toLets (App (Op CDRo) var) xs expr)
	Symbol sym -> prependLet sym var expr
	Pair (x, y) -> case x of
		Symbol sym -> prependLet sym (App (Op FST) var)
				(toLets (App (Op SND) var) y expr)
		ValPattern val -> (toLets (App (Op SND) var) y expr)
		struct -> toLets (App (Op FST) var) x
				(toLets (App (Op SND) var) y expr)
	ValPattern val -> expr

-- preprends a Let onto an expression
-- which correlates to a no recursive alias
prependLet :: Name -> Expr -> Expr -> Expr
prependLet alias varexp expr = Let (NoRec alias [] varexp) expr
