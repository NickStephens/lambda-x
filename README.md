# &lambda;-X 

This project seeks to explore many different fields associated with functional programming and
alternative methods of computation. It uses a lambda calculus abstract syntax, and lambda calculus
parser written in Haskell. This lambda calculus abstract syntax and parser is used as a bridge 
to define the lambda calculus in the lambda calculus. Haskell is our bootstrap. On top of this
the SECD virtual machine architecture will be implemented in Haskell, allowing us to compile our
lambda calculus implementation into to SECD instruction set, where lambda calculus programs will be
runnable.

### Lambda Calculus Parser &larr; Written in Haskell

#### Concrete Syntax:
module &rarr; 'module' alpha '\n' 
<br>
import &rarr; 'import' alpha '\n'
<br>
alias &rarr; lower '=' expression ';'
<br>
expression &rarr; expression expression | abstraction | variable | constant | reduction
<br>
abstraction &rarr; 'L' lower '.' expression
<br>
variable &rarr; lower
<br>
constant &rarr; '#'lower
<br>
lower &rarr; (any series of lowercase characters)
<br>
alpha &rarr; (any series of alphabetic characters)

### Lambda Expression Encoder &larr; Written in Haskell

Encodes lambda terms to a numerical representation, allowing a lambda expression
to evaluate the lambda calculus. This is a requirement, because of the Lambda Calculus's
syntactic purity there is no way to perform beta reduction without syntactically 
distinct representations for both variables and lambda constructs (applications, 
abstractions, and variables). Church-Numerals provide syntactically distinct elements
for an lambda expressions to compare and test against.

The following relation is used to translate a lambda expression (as a Haskell data type)
to list of Church-Numerals representing the expression.

App &rarr; \f.\x. f (f x) &rarr; 2 <br>
Lam &rarr; \f.\x. f x &rarr; 1 <br>
Var &rarr; \f.\x. x &rarr; 0 <br>

Variable names are translated to Church-Numerals as well, and begin with 0. Expressions are encoded as a list would be in the Lambda Calculus.

Example:<br>
\a.\b. a b would be encoded to (1,(0,(1,(1,(2,(0,1))))))

### Lambda Calculus Core &larr; Lambda Calculus
The Core is collection of Lambda Calculus functions. These functions are used to write
a function which takes an encoded Lambda Calculus expression and evaluates it.

The Call-by-Value Evaluation Strategy in the Lambda Calculus:

(\g -> (\x -> (g (x x)) \x -> (g (x x))) \f -> \term -> (((\g -> (\x -> (g (x x)) \x -> (g (x x))) \f -> \i -> \ps -> ((((\p -> (p \a -> \b -> a) (\p ->
(p \a -> \b -> a) ps)) i) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> a) ps))) ((f i) (\p -> (p \a -> \b -> b) ps)))) term) ((\x -> \y -> \p -> ((p
x) y) ((\x -> \y -> \p -> ((p x) y) \arg -> ((\m -> \n -> ((\p -> \q -> ((p q) p) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m ->
\n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) m) n)) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a)
((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) n) m)) \f -> \x -> (f (f (f x)))) (\p -> (p \a -> \b -> a)
arg))) term)) ((\x -> \y -> \p -> ((p x) y) ((\x -> \y -> \p -> ((p x) y) \arg -> ((\m -> \n -> ((\p -> \q -> ((p q) p) ((\m -> \n -> (\n -> ((n \x ->
\a -> \b -> b) \a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) m) n)) ((\m -> \n -> (\n ->
((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) n) m)) \f -> \x ->
(f x)) (\p -> (p \a -> \b -> a) arg))) term)) ((\x -> \y -> \p -> ((p x) y) ((\x -> \y -> \p -> ((p x) y) \arg -> ((\m -> \n -> ((\p -> \q -> ((p q) p)
((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n))
m) n)) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u))
m) m) n)) n) m)) \f -> \x -> (f (f x))) (\p -> (p \a -> \b -> a) arg))) ((\f -> \term -> (((\arg -> ((\m -> \n -> ((\p -> \q -> ((p q) p) ((\m -> \n ->
(\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) m) n)) ((\m
-> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) n)
m)) \f -> \x -> (f x)) (\p -> (p \a -> \b -> a) arg)) (f (\app -> (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) app)) term))) (f ((((\g -> (\x -> (g
(x x)) \x -> (g (x x))) \f -> \m -> \var -> \term -> (((\g -> (\x -> (g (x x)) \x -> (g (x x))) \f -> \i -> \ps -> ((((\p -> (p \a -> \b -> a) (\p -> (p
\a -> \b -> a) ps)) i) ((\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> a) ps)) i)) ((f i) (\p -> (p \a -> \b -> b) ps)))) ((\x -> \y -> \p -> ((p x) y)
term) ((\x -> \y -> \p -> ((p x) y) var) ((\x -> \y -> \p -> ((p x) y) m) ((\x -> \y -> \p -> ((p x) y) f) \x -> \a -> \b -> a))))) ((\x -> \y -> \p ->
((p x) y) ((\x -> \y -> \p -> ((p x) y) \terms -> ((\m -> \n -> ((\p -> \q -> ((p q) p) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m
-> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) m) n)) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b ->
a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) n) m)) \f -> \x -> (f (f (f x)))) (\p -> (p \a -> \b ->
a) (\p -> (p \a -> \b -> a) terms)))) \list -> ((((\m -> \n -> ((\p -> \q -> ((p q) p) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m
-> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) m) n)) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b ->
a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) n) m)) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> a)
(\p -> (p \a -> \b -> b) list)))) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> a) list))) (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) (\p -> (p
\a -> \b -> b) list)))) (\p -> (p \a -> \b -> a) list)))) ((\x -> \y -> \p -> ((p x) y) ((\x -> \y -> \p -> ((p x) y) \terms -> ((\m -> \n -> ((\p -> \q
-> ((p q) p) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u ->
u)) m) m) n)) m) n)) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u ->
x) \u -> u)) m) m) n)) n) m)) \f -> \x -> (f (f x))) (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> a) terms)))) \list -> ((\x -> \y -> \p -> ((p x) y)
\f -> \x -> (f (f x))) ((\x -> \y -> \p -> ((p x) y) ((((\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b ->
b) list)))) (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> b) list)))) (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) list)))
(\app -> (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) app)) (\p -> (p \a -> \b -> a) list)))) ((((\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b)
(\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> b) list)))) (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> b) list)))) (\p -> (p
\a -> \b -> a) (\p -> (p \a -> \b -> b) list))) (\app -> (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> b) app)) (\p -> (p \a -> \b -> a) list))))))) ((\x
-> \y -> \p -> ((p x) y) ((\x -> \y -> \p -> ((p x) y) \terms -> ((\m -> \n -> ((\p -> \q -> ((p q) p) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a
-> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) m) n)) ((\m -> \n -> (\n -> ((n \x -> \a -> \b
-> b) \a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) n) m)) \f -> \x -> (f x)) (\p -> (p \a
-> \b -> a) (\p -> (p \a -> \b -> a) terms)))) \list -> ((((\m -> \n -> ((\p -> \q -> ((p q) p) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b) \a -> \b
-> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) m) n)) ((\m -> \n -> (\n -> ((n \x -> \a -> \b -> b)
\a -> \b -> a) ((\m -> \n -> ((n \n -> \f -> \x -> (((n \g -> \h -> (h (g f))) \u -> x) \u -> u)) m) m) n)) n) m)) (\p -> (p \a -> \b -> b) (\p -> (p \a
-> \b -> a) (\p -> (p \a -> \b -> b) list)))) (\lam -> (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) lam)) (\p -> (p \a -> \b -> a) list))) (\p ->
(p \a -> \b -> a) list)) ((\x -> \y -> \p -> ((p x) y) \f -> \x -> (f x)) ((\x -> \y -> \p -> ((p x) y) (\lam -> (\p -> (p \a -> \b -> a) (\p -> (p \a
-> \b -> b) lam)) (\p -> (p \a -> \b -> a) list))) ((((\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b ->
b) list)))) (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> b) list)))) (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) list)))
(\lam -> (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b -> b) lam)) (\p -> (p \a -> \b -> a) list)))))))) \x -> \a -> \b -> a))))) (f (\app -> (\p -> (p \a
-> \b -> b) (\p -> (p \a -> \b -> b) app)) term))) ((\x -> \y -> \p -> ((p x) y) \f -> \x -> (f (f (f x)))) (\lam -> (\p -> (p \a -> \b -> a) (\p -> (p
\a -> \b -> b) lam)) (f (\app -> (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) app)) term))))) (\lam -> (\p -> (p \a -> \b -> b) (\p -> (p \a -> \b
-> b) lam)) (f (\app -> (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) app)) term)))))) (f ((\x -> \y -> \p -> ((p x) y) \f -> \x -> (f (f x))) ((\x
-> \y -> \p -> ((p x) y) (f (\app -> (\p -> (p \a -> \b -> a) (\p -> (p \a -> \b -> b) app)) term))) (f (\app -> (\p -> (p \a -> \b -> b) (\p -> (p \a
-> \b -> b) app)) term)))))) f) term))) \x -> \a -> \b -> a)))))

### SEC Architecture &larr; Written in Haskell
A virtual machine based off the symbolic architecture of the SEC.

### PCONS &larr; Written in Haskell
A minimalistic, untyped, functional language which compiles to the SEC virtual machine.
