# &lambda;-X 

An attempt to build a universal Lambda Calculus function in the Lambda Calculus.

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
