-- Abstract Syntax for PCONS
module PCONS where

import SEC (Oper (Add, Sub, Mul, Div, Mod, Not, Neg, Lt, Gt, Equ, And, Or, Cdr, Car, Cons, Fst, Snd))

data EXP = 

		Apply EXP EXP | Lambda [Name] EXP | Variable Name | Value AVal |

		Lett Name EXP EXP | Case EXP [(EXP, EXP)] |

		Cond EXP EXP EXP | LetR Name EXP |

		UnOp Oper EXP | BinOp Oper EXP EXP | LSD [EXP] | PR [EXP] |

		CLst [EXP] | Nil | OPR Oper |

		Def Params EXP | RDef Params EXP | Clo EXP |

		TRM EXP | CNT EXP | TNT EXP

				deriving (Show, Eq, Ord)

type Name = String
type Params = [Name]

data AVal = AD Double | AI Integer | AC Char | AB Bool
		deriving (Show, Eq, Ord)

