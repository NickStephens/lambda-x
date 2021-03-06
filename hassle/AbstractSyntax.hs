module AbstractSyntax where

type Program  = [Alias]

type Params = [Pattern]
type Name     = String

data Alias    = NoRec Name Params Expr | Recr Name Params Expr | TRec Name Params Expr
		deriving (Show, Eq, Read)

data Expr     = App Expr Expr | Lam Name Expr | Var Name |
		Case Expr [(Pattern, Expr)] | Let Alias Expr | COND Expr Expr Expr |
		Op Operator | Val Value | Lst [Expr] | Pr (Expr, Expr) |
		Fault
			deriving (Show, Eq, Read)

data Operator = ADD | SUB | MUL | DIV |
		LTo | GTo | ELT | EGT | EQo | NEQ | NOT |
		CDRo | CARo | CONSo |
		FST | SND | PAIRIT |
		AND | OR
		deriving (Show, Eq, Read)

data Pattern  = List (Pattern, Pattern) | Pair (Pattern, Pattern) | Symbol Name | ValPattern Expr
		deriving (Show, Eq, Read)

data Value    = ValInt Integer | ValDouble Double |
		ValBool Bool | ValChar Char 
					deriving (Show, Eq, Read)

data Number   = NumInt Int | NumDouble Double
		deriving (Show, Eq, Read)
