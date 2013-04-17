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
reduction &rarr; '@normal(' expression ')' | '@applicative(' expression ')'

lower &rarr; (any series of lowercase characters)
<br>
alpha &rarr; (any series of alphabetic characters)

### Lambda Expression Encoder &larr; Written in Haskell
### Lambda Calculus Core &larr; Lambda Calculus

### SECD Architecture &larr; Written in Lambda Calculus
