module AbstractSyntax where

type Program  = [Alias]

type Name     = String
data Alias    = NoRec Name [Name] Expr | Rec Name [Name] Expr | TRec Name [Name] Expr
		deriving (Show, Eq)

data Expr     = App Expr Expr | Lam Name Expr | Var Name |
		Case Expr [(Pattern, Expr)] | Let Alias Expr |
		Op Operator | Val Value 		deriving (Show, Eq)

data Operator = ADD | SUB | MUL | DIV |
		LTo | GTo | ELT | EGT | EQo | NEQ | NOT |	
		CDR | CAR | CONSo
		deriving (Show, Eq)

data Pattern  = List (Name, Name) | Pair (Name, Name)
		deriving (Show, Eq)

data Value    = ValInt Integer | ValDouble Double | ValPair (Expr, Expr) | ValList [Expr] | ValBool Bool | ValChar Char
		deriving (Show, Eq)

data Number   = NumInt Int | NumDouble Double
		deriving (Show, Eq)
