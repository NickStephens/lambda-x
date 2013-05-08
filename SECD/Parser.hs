module Parser where 

import Prelude hiding (LT, GT, EQ, div)
import Data.Char
import AbstractSyntax 
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Expr as EX
import Text.Parsec.Error

main file = do 
	f <- parseFromFile program file
	case (f) of
		Left err -> print err
		Right res -> print res

{- PROGRAM -}

program :: Parser Program
program = do
		prog <- alias `endBy` (char ';' >> many space)
		eof
		return prog

{- ALIAS -}

alias = try (trec) <|> try (rec) <|> norec 

norec = do
	(als, params, exp) <- acenter
	return $ NoRec als params exp

trec = do
	string "Tletrec"
	many1 space
	(als, params, exp) <- acenter
	return $ TRec als params exp

rec = do
	string "Letrec"
	many1 space
	(als, params, exp) <- acenter
	return $ Rec als params exp

acenter = do
	als <- name
	many1 space
	params <- name `sepEndBy` (char ' ')
	many space
	char '='
	many space
	exp <- expression
	return $ (als, params, exp)

{- EXPRESSION -}
-- binary sugar and operator application has higher precedence 
-- then functional application

{-
expression = EX.buildExpressionParser table term

table = [ [EX.Infix application EX.AssocLeft],
	  [EX.Infix (binary mulop) EX.AssocLeft],
	  [EX.Infix (binary addop) EX.AssocLeft],
	  [EX.Infix (binary cons) EX.AssocLeft, EX.Prefix (unary car), EX.Prefix (unary cdr)],
	  [EX.Infix (binary relop) EX.AssocLeft, EX.Prefix (unary notop)] ]
-}

expression = stratum `chainl1` (binary relop)
stratum    = primary `chainl1` (binary cons)
primary    = factor  `chainl1` (binary addop)
factor	   = app     `chainl1` (binary mulop)
app 	   = term `chainl1` application 
term = try (caseof) <|> try (conditional) <|> try (parexpression) <|> paroperator <|>  variable <|> lambda <|> value

{- PARENTHESIZED EXPRESSION -}
parexpression = do
		char '('
		many space
		e <- expression
		many space
		char ')'
		return e

paroperator = do
		char '('
		many space
		op <- operator
		many space
		char ')'
		return op

{- SUGAR -}
binary f = do
	many space
	op <- f
	many space
	return (\x -> \y -> (App (App op x) y))

unary f = do
	op <- f
	return (\x -> (App op x))

{- APPLICATION -}
application = do
	many1 space
	return App

{- LAMBDA ABSTRACTION -}
lambda = do
	char '\\'
	many space
	nm <- name
	many space
	char '.'
	many space
	test <- expression 
	return $ Lam nm test

{- VARIABLE -}

variable = do
	nm <- name
	return $ Var nm

{- LET -}

plet :: Parser Expr
plet = do
	string "let"
	many1 space
	als <- alias
	char ';'
	many space
	string "in"
	many1 space
	exp <- expression
	return $ Let als exp

{- CONDITIONALS -}

conditional = do
	string "if"
	many1 space
	bool <- parexpression
	many1 space
	string "then"
	many1 space
	branch1 <- parexpression
	many1 space
	string "else"	
	many1 space
	branch2 <- parexpression
	return $ Cond bool branch1 branch2

{- CASE OF -}
caseof = do
	string "case"
	many1 space
	test <- parexpression
	many1 space
	string "of"
	many1 space
	col <- cases
	return $ Case test  col	

cases = many1 pcase

pcase = do
	pat <- pattern
	many space
	string "->"	
	many space
	exp <- parexpression
	many space
	return (pat, exp)

{- PATTERNS -}

pattern = try (listpattern) <|> pairpattern <|> symbol

listpattern = do
	char '('
	many space
	head <- name	
	many space
	char ':'
	many space
	tail <- name
	char ')'
	return $ List (head, tail)

pairpattern = do
	char '('
	many space
	fst <- name
	many space
	char ','
	many space
	snd <- name
	char ')'	
	return $ Pair (fst, snd)

symbol = do
	nm <- name
	return $ Symbol nm

{- VALUES -}

value = pair <|> list <|> integer <|> pchar <|> bool

pair = do
	char '('
	many space
	e1 <- expression
	char ','
	many space
	e2 <- expression
	char ')'
	return $ Val $ ValPair (e1, e2)

list = do
	char '['
	elst <- (many space >> expression) `sepBy` (char ',')
	char ']'
	return $ Val $ ValList elst

number = try (double) <|> integer

integer :: Parser Expr
integer = do
	int <- many1 digit
	return $ Val . ValInt $ toInt 0 int

double :: Parser Expr
double = do
	head <- many1 digit
	char '.'
	tail <-	many1 digit
	return $ Val . ValDouble $ (fromIntegral $ toInt 0 (head ++ tail)) / (10^(length tail))
	
toInt n [] = n
toInt n (s:tr) = toInt (10*n + digitToInt s) tr

bool :: Parser Expr
bool = do
	bl <- (string "true" <|> string "false")
	return bl
	return $ Val . ValBool $ if bl == "true" then True else False

pchar :: Parser Expr
pchar = do
	char '\''
	c <- anyChar
	char '\''
	return $ Val $ ValChar c

{- OPERATORS -}

operator = do
	relop <|> addop <|> mulop <|> listop <?> "operator"

relop = do
	try (elt) <|> try (egt) <|> lt <|> gt <|> eq <|> neq
	<?> "relational operator"

listop = do
	car <|> cdr <?> "list operator"

addop = do
	add <|> sub <?> "addition operator"

mulop = do
	mul <|> div <?> "multiplicative operator"

-- ARITHMENTIC OPERATORS
add = do
	char '+'
	return $ Op ADD


sub = do
	char '-'
	return $ Op SUB


mul = do
	char '*'
	return $ Op MUL


div = do
	char '/'
	return $ Op DIV 

-- RELATIONAL OPERATORS

lt = do
	char '<'
	return $ Op LT

gt = do
	char '>'
	return $ Op GT 

elt = do
	string "<="
	return $ Op ELT

egt = do
	string ">="
	return $ Op EGT

eq = do
	string "=="
	return $ Op EQ

neq = do
	string "/="
	return $ Op NEQ

notop :: Parser Expr
notop = do
	string "!"
	return $ Op NOT

-- LIST OPERATORS

car = do
	char '^'
	return $ Op CAR


cdr = do
	char '~'
	return $ Op CDR


cons = do
	char ':'
	return $ Op CONS 

{- CASE -}

{- VALUES -}

{- INTERMEDIATE PARSING -}
name :: Parser Name
name = do
	head <- letter
	tail <- many alphaNum
	return (head:tail)

eol = do { try (string "\n\r") <|> try (string "\r\n") 
	  <|> string "\r" <|> string "\n"
	 ; return ()
	 }

end = do 
	eol <|> (many space >> eof)
