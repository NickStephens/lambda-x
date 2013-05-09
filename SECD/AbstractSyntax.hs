module AbstractSyntax where

type Program  = [Alias]

type Name     = String
data Alias    = NoRec Name [Name] Expr | Recr Name [Name] Expr | TRec Name [Name] Expr
		deriving (Show, Eq, Read)

data Expr     = App Expr Expr | Lam Name Expr | Var Name |
		Case Expr [(Pattern, Expr)] | Let Alias Expr | COND Expr Expr Expr |
		Op Operator | Val Value | TERM | Lst [Expr] | Pr [Expr]
			deriving (Show, Eq, Read)

data Operator = ADD | SUB | MUL | DIV |
		LTo | GTo | ELT | EGT | EQo | NEQ | NOT |	
		CDRo | CARo | CONSo
		deriving (Show, Eq, Read)

data Pattern  = List (Name, Name) | Pair (Name, Name) | Symbol Name
		deriving (Show, Eq, Read)

data Value    = ValInt Integer | ValDouble Double |
				ValBool Bool | ValChar Char
					deriving (Show, Eq, Read)

data Number   = NumInt Int | NumDouble Double
		deriving (Show, Eq, Read)
