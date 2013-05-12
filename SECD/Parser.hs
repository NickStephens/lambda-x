module Parser where 

import Prelude hiding (LT, GT, EQ, div)
import Data.Char
import AbstractSyntax 
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Expr as EX
import Text.Parsec.Error

mane file = do 
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
	return $ Recr als params exp

acenter = do
	als <- name
	many1 space
	params <- pattern `sepEndBy` (char ' ')
	many space
	char '='
	many space
	exp <- expression
	return $ (als, params, exp)

{- EXPRESSION -}
-- binary sugar and operator application has higher precedence 
-- then functional application

expression = logical `chainl1` (binary logop)
logical    = stratum `chainl1` (binary relop) <|> unary (notop) stratum
stratum    = primary `chainl1` (binary cons) <|> unary (car <|> cdr) primary
primary    = factor  `chainl1` (binary addop)
factor	   = app     `chainl1` (binary mulop)
app 	   = term `chainl1` application 
term = try (plet) <|> try (conditional) <|> try (caseof) 
		<|> try (parexpression) <|> try (paroperator) <|> try (value) 
		<|> variable <|> lambda 

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

unary f par = do
	op <- f
	exp <- par
	return (App op exp)

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
	return $ COND bool branch1 branch2

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

pattern = try (listpattern) <|> try (pairpattern) <|> symbol <|> valuePattern

listpattern = do
        char '('
        many space
        head <- pattern
        many space
        char ':'
        many space
        tail <- pattern
        char ')'
        return $ List (head, tail)

pairpattern = do
        char '('
	many space
	p1 <- pattern 
	char ','
	many space
	p2 <- pattern 
        char ')'
        return $ Pair (p1, p2)

symbol = do
        nm <- name
        return $ Symbol nm

valuePattern = do
		val <- value
		return $ ValPattern val


{- VALUES -}

value = pair <|> list <|> integer <|> pchar <|> bool

pair = do
	char '('
	many space
	elst <- (many space >> expression) `sepBy` (char ',')
	char ')'
	return $ Pr elst

list = do
	char '['
	elst <- (many space >> expression) `sepBy` (char ',')
	char ']'
	return $ Lst elst

number = try (double) <|> integer

integer :: Parser Expr
integer = do
	int <- many1 digit
	return $ Val . ValInt $ toInteger $ toInt 0 int

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

operator = relop <|> addop <|> mulop <|> listop <|> logop <?> "operator"

logop = andop <|> orop <?> "logical operator"

relop = try (elt) <|> try (egt) <|> lt <|> gt <|> eq <|> neq
	<?> "relational operator"

listop = car <|> cdr <?> "list operator"

addop = add <|> sub <?> "addition operator"

mulop = mul <|> div <?> "multiplicative operator"

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
	return $ Op LTo

gt = do
	char '>'
	return $ Op GTo

elt = do
	string "<="
	return $ Op ELT

egt = do
	string ">="
	return $ Op EGT

eq = do
	string "=="
	return $ Op EQo

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
	return $ Op CARo

cdr = do
	char '~'
	return $ Op CDRo


cons = do
	char ':'
	return $ Op CONSo

-- LOGICAL OPERATORS

andop = do 
	string "&&" 
	return $ Op AND

orop = do
	string "||"
	return $ Op OR

-- PAIR OPERATORS

fstop :: Parser Expr
fstop = do
	char '$'
	return $ Op FST
	
sndop :: Parser Expr
sndop = do
	char '#'
	return $ Op SND

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
