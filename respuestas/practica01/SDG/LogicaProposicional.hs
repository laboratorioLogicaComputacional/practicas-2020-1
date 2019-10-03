-- 2 . LOGICA PROPOSICIONAL

-- Tipo de dato indice
type Indice = Int
-- Tipo de dato fórmula
data PL = Top | Bot | Var Indice
    | Oneg PL
    | Oand PL PL | Oor PL PL
    | Oimp PL PL deriving (Eq, Show)

-- 2.1 Dada una fórmula regresa un valor de verdad si hay una implicación en dicha fórmula.
hayImplicacion :: PL -> Bool
hayImplicacion phi = case phi of 
    Top -> False
    Bot -> False
    Var i -> False
    Oneg alpha -> hayImplicacion alpha
    Oand alpha beta -> (hayImplicacion alpha) || (hayImplicacion beta)
    Oor alpha beta -> (hayImplicacion alpha) || (hayImplicacion beta)
    Oimp alpha beta -> True 


-- 2.2 Dada una fórmula regresar una lista con las disyunciones de dicha fórmula.
disy :: PL -> [PL]
disy phi = case phi of 
    Top -> []
    Bot -> []
    Var i -> []
    Oneg alpha -> disy alpha
    Oand alpha beta -> (disy alpha) ++ (disy beta)
    Oor alpha beta -> [phi]
    Oimp alpha beta -> (disy alpha) ++ (disy beta) 

-- 2.3 Dada una formula nos regresa el numero de conjunciones en esa formula.
numConj :: PL -> Int
numConj phi = case phi of 
    Top -> 0
    Bot -> 0
    Var i -> 0
    Oneg alpha -> numConj alpha
    Oand alpha beta -> 1 + (numConj alpha) + (numConj beta)
    Oor alpha beta -> (numConj alpha) + (numConj beta)
    Oimp alpha beta -> (numConj alpha) + (numConj beta)        