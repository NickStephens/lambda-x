module AbstractSyntax where

type Program  = [Alias]

type Name     = String
data Alias    = Alias Name [Name] Expr
		deriving (Show, Eq)

data Expr     = App Expr Expr | Lam Name Expr | Var Name |
		Case Expr [(Pattern, Expr)] | Let Alias Expr |
		Op Operator | Val Value |
		TERM -- Expression Terminator
		deriving (Show, Eq)

data Operator = ADD | SUB | MUL | DIV |
		LT | GT | ELT | EGT | EQ | NEQ | NOT |	
		CDR | CAR | CONS 
		deriving (Show, Eq)

data Pattern  = List (Name, Name) | Pair (Name, Name)
		deriving (Show, Eq)

data Value    = ValInt Int | ValDouble Double | ValPair (Expr, Expr) | ValList [Expr] | ValBool Bool | ValChar Char
		deriving (Show, Eq)

data Number   = NumInt Int | NumDouble Double
		deriving (Show, Eq)
