module Desugarer where

import Parser 
import AbstractSyntax
import Text.ParserCombinators.Parsec (parse, many)
import Control.Monad.State

debugcs expr = case (parse expression "" expr) of
	Right res -> caseofDesugar res

debugal expr = case (parse alias "" expr) of
	Right res -> aliasDesugar res

paramTest pat = case (parse (many pattern) "" pat) of
	Right res -> placeParams res (Fault)

-- Allows us to write a more proper translator
type DesugaredProgram = [DesugaredAlias]

data DesugaredAlias = DNoRec Name Expr | DRecr Name Expr | DTRec Name Expr
		deriving (Show)

{- ALIAS DESUGARING -}

aliasDesugar :: Alias -> DesugaredAlias
aliasDesugar alias = case alias of
	NoRec nm params expr ->
		DNoRec nm (placeParams params expr)
	Recr nm params expr ->
		DRecr nm (placeParams params expr)
	TRec nm params expr ->
		DTRec nm (placeParams params expr)

placeParams = placeParams' 0 

placeParams' :: Int -> [Pattern] -> Expr -> Expr
placeParams' cnt [] expr = expr
placeParams' cnt (p:params) expr = case p of
		Symbol sym -> Lam sym $ placeParams' (cnt + 1) params expr
		ValPattern val -> Lam strcnt $ COND (App (App (Op NEQ) 
			(Var $ "v" ++ strcnt)) val) Fault $ 
				placeParams' (cnt + 1)  params expr
		List (x,xs) -> Lam ("lst" ++ strcnt) $ toLets (Var ("lst" ++ strcnt)) p $ 
				placeParams' (cnt + 1)  params expr 
		Pair (x,y) -> Lam ("pr" ++ strcnt) $ toLets (Var ("pr" ++ strcnt)) p $  
				placeParams' (cnt + 1)  params expr
		where strcnt = show cnt

{- CASEOF DESUGARING -}

-- any pattern matching on structures needs to account
-- for values existing within those structures
caseofDesugar :: Expr -> Expr
caseofDesugar (Case subject []) = Fault --temp solution
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
		      	( valMatch subject (fst b) 
			(Val (ValBool True)))
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
