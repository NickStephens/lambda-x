-- Abstract Syntax for PCONS
module PCONS where

import AbstractSyntax (Pattern)
import SEC (Oper (Add, Sub, Mul, Div, Mod, Not, Neg, Lt, Gt, Equ, And, Or, Cdr, Car, Cons))

data EXP = 
		Apply EXP EXP | Lambda [Name] EXP | Variable Name | Value AVal |

		Lett Name EXP EXP | Case EXP [(EXP, EXP)] | Cond EXP EXP EXP |

		UnOp Oper EXP | BinOp Oper EXP EXP | LSD [EXP] | PR [EXP] |

		CLst [EXP] | Nil | OPR Oper | ConS EXP EXP |

		Def Params EXP | RDef Params EXP | Clo EXP | Rec Name | 

		TRM EXP | TRCL EXP EXP EXP | RCL EXP EXP EXP | CNT EXP | TNT EXP | Skp

				deriving (Show, Eq, Ord)

type Name = String

data PATTERN = LISTP (Name, Name) | PAIRP (Name, Name) | SYMP Name
				deriving (Show, Eq, Ord)

type Params = [Name]

data AVal = AD Double | AI Integer | AC Char | AB Bool
		deriving (Show, Eq, Ord)
