## Lambda-X 

An attempt to build a universal Lambda Calculus function in the Lambda Calculus.

### Lambda Calculus Parser &larr; Written in Haskell

#### Concrete Syntax:
alias &rarr; lower '=' expression ';'
<br>
expression &rarr; expression expression | abstraction | variable | constant
<br>
abstraction &rarr; 'L' lower '.' expression
<br>
variable &rarr; lower
<br>
constant &rarr; '#'lower

lower &rarr; (any series of lowercase characters)

### Lambda Expression Encoder &larr; Written in Haskell
### Lambda Calculus Core &larr; Lambda Calculus

### SECD Architecture &larr; Written in Lambda Calculus
