module LambdaCore where
import Lambda
import LamParsec

tr = (Lam "a" (Lam "b" (Var "a")))
fl = (Lam "a" (Lam "b" (Var "b")))

iF = (Lam "p" (Lam "a" (Lam "b" (App (App (Var "p") (Var "a")) (Var "b")))))
oR = (Lam "p" (Lam "q" (App (App (Var "p") (Var "p")) (Var "q"))))
anD = (Lam "p" (Lam "q" (App (App (Var "p") (Var "q")) (Var "p"))))
noT = (Lam "m" (Lam "a" (Lam "b" (App (App (Var "m") (Var "b")) (Var "a")))))

empty = fl
isEmpty = (Lam "l" (App (App (Var "l") (Lam "a" (Lam "b" fl))) tr))
cons = (Lam "h" (Lam "t" (Lam "c" (Lam "n" (App (App (Var "c") (Var "h")) (App (App (Var "t") (Var "c")) (Var "n")))))))
hd = (Lam "l" (App (App (Var "l") tr) fl))
tl = (Lam "l" (App frst (App (App (Var "l") (Lam "a" (Lam "b" (App (App pr (App scnd (Var "b"))) (App (App cons (Var "a")) (App scnd (Var "b"))))))) (App (App pr empty) empty))))

ap = (Lam "f" (Lam "a" (Lam "b" (App (App (App iF (App isEmpty (Var "a"))) (Var "b")) (App (App cons (App hd (Var "a"))) (App (App (Var "f") (App tl (Var "a"))) (Var "b")))))))
apnd = (Lam "a" (Lam "b" (App (App (App y ap) (Var "a")) (Var "b"))))

qsy = (Lam "f" (Lam "as" (App (App (App iF (App isEmpty (Var "as"))) empty) (App (App apnd (App (App apnd (App (Var "f") (App (App les (App hd (Var "as"))) (App tl (Var "as"))))) (App (App cons (App hd (Var "as"))) empty))) (App (Var "f") (App (App gts (App hd (Var "as"))) (App tl (Var "as"))))))))
qsort = (Lam "as" (App (App y qsy) (Var "as")))
lesy = (Lam "f" (Lam "a" (Lam "as" (App (App (App iF (App isEmpty (Var "as"))) empty) (App (App (App iF (App (App lte (App hd (Var "as"))) (Var "a"))) (App (App cons (App hd (Var "as"))) (App (App (Var "f") (Var "a")) (App tl (Var "as"))))) (App (App (Var "f") (Var "a")) (App tl (Var "as"))))))))
les = (Lam "a" (Lam "as" (App (App (App y lesy) (Var "a")) (Var "as"))))
gtsy = (Lam "f" (Lam "a" (Lam "as" (App (App (App iF (App isEmpty (Var "as"))) empty) (App (App (App iF (App (App gt (App hd (Var "as"))) (Var "a"))) (App (App cons (App hd (Var "as"))) (App (App (Var "f") (Var "a")) (App tl (Var "as"))))) (App (App (Var "f") (Var "a")) (App tl (Var "as"))))))))
gts = (Lam "a" (Lam "as" (App (App (App y gtsy) (Var "a")) (Var "as"))))

car = (Lam "p" (App (Var "p") tr))
cdr = (Lam "p" (App (Var "p") fl))
nil = (Lam "x" tr)
nul = (Lam "p" (App (Var "p") (Lam "x" (Lam "y" fl))))
papp = (App y (Lam "g" (Lam "a" (Lam "b" (App (App (App nul (Var "a")) (Var "b")) (App (App pr (App car (Var "a"))) (App (App (Var "g") (App cdr (Var "a"))) (Var "b"))))))))
plen = (App (App y (Lam "g" (Lam "c" (Lam "x" (App (App (App nul (Var "x")) (Var "c")) (App (App (Var "g") (App sucs (Var "c"))) (App cdr (Var "x")))))))) zero)
pidx = (Lam "x" (Lam "i" (App car (App (App (Var "i") cdr) (Var "x")))))
plast = (App y (Lam "g" (Lam "x" (App (App (App nul (Var "x")) nil) (App (App (App nul (App cdr (Var "x"))) (App car (Var "x"))) (App (Var "g") (App car (Var "x"))))))))
prvr = (App (App y (Lam "g" (Lam "a" (Lam "l" (App (App (App nul one) (Var "a")) (App (App (Var "g") (App (App pr (App car (Var "one"))) (Var "a"))) (App cdr (Var "one")))))))) nil)
pqsy = (Lam "f" (Lam "as" (App (App (App iF (App nul (Var "as"))) nil) (App (App papp (App (Var "f") (App (App ples (App car (Var "as"))) (App cdr (Var "as"))))) (App (App papp (App (App pr (App car (Var "as"))) nil)) (App (Var "f") (App (App gts (App car (Var "as"))) (App cdr (Var "as")))))))))
pqsort = (Lam "as" (App (App y pqsy) (Var "as")))
plesy = (Lam "f" (Lam "a" (Lam "as" (App (App (App iF (App nul (Var "as"))) nil) (App (App (App iF (App (App lte (App car (Var "as"))) (Var "a"))) (App (App pr (App car (Var "as"))) (App (App (Var "f") (Var "a")) (App cdr (Var "as"))))) (App (App (Var "f") (Var "a")) (App cdr (Var "as"))))))))
ples = (Lam "a" (Lam "as" (App (App (App y plesy) (Var "a")) (Var "as"))))
pgtsy = (Lam "f" (Lam "a" (Lam "as" (App (App (App iF (App nul (Var "as"))) nil) (App (App (App iF (App (App gt (App car (Var "as"))) (Var "a"))) (App (App pr (App car (Var "as"))) (App (App (Var "f") (Var "a")) (App cdr (Var "as"))))) (App (App (Var "f") (Var "a")) (App cdr (Var "as"))))))))
pgts = (Lam "a" (Lam "as" (App (App (App y pgtsy) (Var "a")) (Var "as"))))

pr = (Lam "x" (Lam "y" (Lam "p" (App (App (Var "p") (Var "x")) (Var "y")))))
frst = (Lam "p" (App (Var "p") tr))
scnd = (Lam "p" (App (Var "p") fl))

y = (Lam "g" (App (Lam "x" (App (Var "g") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "g") (App (Var "x") (Var "x"))))))

zero = (Lam "f" (Lam "x" (Var "x")))
one = (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
two = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x")))))
three = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))
four = (Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))))

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

casE = (Lam "i" (Lam "p" (App (App (App y cs) (Var "i")) (Var "p"))))
cs = (Lam "f" (Lam "i" (Lam "p" (App (App (App iF (App (App eq (App frst (App hd (Var "p")))) (Var "i"))) (App scnd (App hd (Var "p")))) (App (App (Var "f") (Var "i")) (App tl (Var "p")))))))

ptrn = (App (App pr zero) tr)
ptrni = (App (App pr one) fl)
lst = (App (App cons ptrn) (App (App cons ptrni) empty))
test = (App (App casE zero) lst)
tst = (Lam "a" (Lam "b" (Lam "c" (Lam "d" (Lam "e" (App (App (App (App (Var "a") (Var "b")) (Var "c")) (Var "d")) (Var "e")))))))
alist = (App (App cons one) (App (App cons two) empty))
blist = (App (App cons three) (App (App cons four) empty))
qlist = (App (App cons three) (App (App cons one) (App (App cons zero) (App (App cons four) empty))))
qtest = (Lam "as" (App (App apnd (App (App apnd (App (App les (App hd (Var "as"))) (App tl (Var "as")))) (App (App cons (App hd (Var "as"))) empty))) (App (App gts (App hd (Var "as"))) (App tl (Var "as")))))
plista = (App (App pr zero) (App (App pr one) (App (App pr two) nil)))
plistb = (App (App pr three) (App (App pr four) nil))
pqlist = (App (App pr three) (App (App pr one) (App (App pr zero) (App (App pr four) nil))))
pless = (Lam "as" (App (App ples (App car (Var "as"))) (App cdr (Var "as"))))
pgtss = (Lam "as" (App (App pgts (App car (Var "as"))) (App cdr (Var "as"))))
ppen = (Lam "as" (Lam "bs" (Lam "scs" (App (App papp (Var "as")) (App (App papp (Var "bs")) (Var "scs"))))))
