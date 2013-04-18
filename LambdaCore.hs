module LambdaCore where
import Lambda
import LamParsec

tr = (Lam "a" (Lam "b" (Var "a")))
fl = (Lam "a" (Lam "b" (Var "b")))

iF = (Lam "p" (Lam "a" (Lam "b" (App (App (Var "p") (Var "a")) (Var "b")))))
oR = (Lam "p" (Lam "q" (App (App (Var "p") (Var "p")) (Var "q"))))
anD = (Lam "p" (Lam "q" (App (App (Var "p") (Var "q")) (Var "p"))))
noT = (Lam "m" (Lam "a" (Lam "b" (App (App (Var "m") (Var "b")) (Var "a")))))

cons = pr
car = (Lam "p" (App (Var "p") tr))
cdr = (Lam "p" (App (Var "p") fl))
nil = (Lam "x" tr)
nul = (Lam "p" (App (Var "p") (Lam "x" (Lam "y" fl))))
appnd = (App y (Lam "g" (Lam "a" (Lam "b" (App (App (App nul (Var "a")) (Var "b")) (App (App pr (App car (Var "a"))) (App (App (Var "g") (App cdr (Var "a"))) (Var "b"))))))))
len = (App (App y (Lam "g" (Lam "c" (Lam "x" (App (App (App nul (Var "x")) (Var "c")) (App (App (Var "g") (App sucs (Var "c"))) (App cdr (Var "x")))))))) zero)
idx = (Lam "x" (Lam "i" (App car (App (App (Var "i") cdr) (Var "x")))))
last = (App y (Lam "g" (Lam "x" (App (App (App nul (Var "x")) nil) (App (App (App nul (App cdr (Var "x"))) (App car (Var "x"))) (App (Var "g") (App cdr (Var "x"))))))))
rvrs = (App (App y (Lam "g" (Lam "a" (Lam "l" (App (App (App nul (Var "l")) (Var "a")) (App (App (Var "g") (App (App pr (App car (Var "l"))) (Var "a"))) (App cdr (Var "l")))))))) nil)
fold = (Lam "f" (Lam "e" (Lam "x" (App (App y (Lam "g" (Lam "y" (App (App (App nul (Var "y")) (Var "e")) (App (App (Var "f") (App car (Var "y"))) (App (Var "g") (App cdr (Var "y")))))))) (Var "x")))))
ins = (App y (Lam "f" (Lam "a" (Lam "as" (App (App (App iF (App (App oR (App nul (Var "as"))) (App (App lte (Var "a")) (App car (Var "as"))))) (App (App pr (Var "a")) (Var "as"))) (App (App pr (App car (Var "as"))) (App (App (Var "f") (Var "a")) (App cdr (Var "as")))))))))
insort = (App y (Lam "f" (Lam "as" (Lam "bs" (App (App (App nul (Var "as")) (Var "bs")) (App (App (Var "f") (App cdr (Var "as"))) (App (App ins (App car (Var "as"))) (Var "bs"))))))))
insert = (Lam "l" (App (App (App fold ins) nil) (Var "l")))

pr = (Lam "x" (Lam "y" (Lam "p" (App (App (Var "p") (Var "x")) (Var "y")))))
frst = (Lam "p" (App (Var "p") tr))
scnd = (Lam "p" (App (Var "p") fl))

y = (Lam "g" (App (Lam "x" (App (Var "g") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "g") (App (Var "x") (Var "x"))))))

zero = (Lam "f" (Lam "x" (Var "x")))
one = (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
two = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x")))))
three = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))
four = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))))
five = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))))

sucs = (Lam "n" (Lam "f" (Lam "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))))))
prdc = (Lam "n" (Lam "f" (Lam "x" (App (App (App (Var "n") (Lam "g" (Lam "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Lam "u" (Var "x"))) (Lam "u" (Var "u"))))))
isZr = (Lam "n" (App (App (Var "n") (Lam "x" fl)) tr))
subt = (Lam "m" (Lam "n" (App (App (Var "n") prdc) (Var "m"))))
plus = (Lam "m" (Lam "n" (Lam "f" (Lam "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x")))))))
mult = (Lam "m" (Lam "n" (Lam "f" (App (Var "m") (App (Var "n") (Var "f"))))))
exp = (Lam "m" (Lam "n" (App (Var "n") (Var "m"))))
lte = (Lam "m" (Lam "n" (App isZr (App (App subt (Var "m")) (Var "n")))))
eq = (Lam "m" (Lam "n" (App (App anD (App (App lte (Var "m")) (Var "n"))) (App (App lte (Var "n")) (Var "m")))))
gt = (Lam "m" (Lam "n" (App noT (App (App lte (Var "m")) (Var "n")))))
lt = (Lam "m" (Lam "n" (App (App anD (App (App lte (Var "m")) (Var "n"))) (App noT (App (App eq (Var "m")) (Var "n"))))))

cas = (Lam "i" (Lam "cs" (App (App (App y cs) (Var "i")) (Var "cs"))))
cs = (Lam "f" (Lam "i" (Lam "p" (App (App (App (App eq (App frst (App car (Var "p")))) (Var "i")) (App scnd (App car (Var "p")))) (App (App (Var "f") (Var "i")) (App cdr (Var "p")))))))
casE = (App y (Lam "f" (Lam "i" (Lam "ps" (App (App (App (App eq (App frst (App car (Var "ps")))) (Var "i")) (App scnd (App car (Var "ps")))) (App (App (Var "f") (Var "i")) (App cdr (Var "ps"))))))))


sss = (Lam "xs" (Lam "ys" (App (App (Lam "x" (Lam "y" (App (App (App (App (Lam "x" (Lam "y" (App (App lt (Var "x")) (Var "y")))) (Var "x")) (Var "y")) one) (App (App (App (App (Lam "x" (Lam "y" (App (App eq (Var "x")) (Var "y")))) (Var "x")) (Var "y")) two) three)))) (App car (Var "xs"))) (App car (Var "ys")))))
unHd = (App y (Lam "f" (Lam "xs" (Lam "ys" (Lam "ac" (App (App (Lam "x" (Lam "y" (App (App (App nul (Var "xs")) (App (App insort (Var "ys")) (Var "ac"))) (App (App (App nul (Var "ys")) (App (App insort (Var "xs")) (Var "ac"))) (App (App (App (App (Lam "x" (Lam "y" (App (App lt (Var "x")) (Var "y")))) (Var "x")) (Var "y")) (App (App (App (Var "f") (App cdr (Var "xs"))) (Var "ys")) (App (App ins (Var "x")) (Var "ac")))) (App (App (App (App (Lam "x" (Lam "y" (App (App eq (Var "x")) (Var "y")))) (Var "x")) (Var "y")) (App (App (App (Var "f") (App cdr (Var "xs"))) (App cdr (Var "ys"))) (App (App ins (Var "x")) (Var "ac")))) (App (App (App (Var "f") (Var "xs")) (App cdr (Var "ys"))) (App (App ins (Var "y")) (Var "ac"))))))))) (App car (Var "xs"))) (App car (Var "ys"))))))))
union = (Lam "xs" (Lam "ys" (App (App (App unHd (Var "xs")) (Var "ys")) nil)))

ptrn = (App (App pr zero) tr)
ptrni = (App (App pr one) fl)
test = (App (App casE zero) tst)
tst = (App (App cons ptrn) (App (App cons ptrni) (App cons nil)))
lista = (App (App pr zero) (App (App pr one) (App (App pr two) nil)))
listb = (App (App pr three) (App (App pr four) nil))
duplist = (App (App cons zero) (App (App cons zero) (App (App cons one) (App (App cons one) (Var "nil")))))
list = (App (App pr five) (App (App pr three) (App (App pr four) (App (App pr two) (App (App pr one) (App (App pr zero) (App (App pr three) (App (App pr four) (App (App pr one) (App (App pr five) (App (App pr zero) (App (App pr two) nil))))))))))))
ppen = (Lam "as" (Lam "b" (Lam "scs" (App (App appnd (Var "as")) (App (App appnd (App (App pr (Var "b")) nil)) (Var "scs"))))))
sing = (App (App pr one) nil)
tet = (App (App pr one) (App (App pr two) (App (App pr three) (App (App pr (Var "five")) (App (App pr four) nil)))))
tat = (App (App pr four) (App (App pr three) (App (App pr two) (App (App pr one) nil))))
tot = (App (App pr zero) (App (App pr one) (App (App pr three) (App (App pr four) nil))))
mane = (App (App insort list) (App (App insort list) nil))
man = (App insert list)
