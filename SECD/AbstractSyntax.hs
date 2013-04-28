module AbstractSyntax where

data Program  = Program [Alias]

type Name     = String
data Alias    = Def Name [Name] Expr
		deriving (Show, Eq)

data Expr     = App Expr Expr | Lam Name Expr | Var Name |
		Case Expr [(Pattern, Expr)] | Let Alias Expr |
		Op Operator | Val Value
		deriving (Show, Eq)

data Operator = ADD | SUB | MUL | DIV |
		LT | GT | ELT | EGT | EQ | NEQ |	
		CDR | CAR | NUL 
		deriving (Show, Eq)

data Pattern  = List (Name, Name) | Pair (Name, Name)
		deriving (Show, Eq)

data Value    = ValNumber Number | ValList [Expr] | ValBool Bool | ValChar Char
		deriving (Show, Eq)

data Number   = NumInt Int | NumFloat Float 
		deriving (Show, Eq)
