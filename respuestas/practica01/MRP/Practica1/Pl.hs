module Pl
  where

-- | Tipo de dato indice

type Indice = Int

-- | Tipo de dato fórmula
data PL = Top | Bot
              | Var Indice
              | Oneg PL
              | Oand PL PL  
              | Oor  PL PL
              | Oimp PL PL deriving (Eq, Show)

hayImplicacion :: PL -> Bool
hayImplicacion phi =  case phi of
  Top -> False
  Bot -> False
  Var v -> False
  Oneg alpha -> hayImplicacion alpha
  Oand alpha beta -> hayImplicacion alpha || hayImplicacion beta
  Oor alpha beta -> hayImplicacion alpha || hayImplicacion beta
  Oimp alpha beta -> True

disy :: PL -> [PL]
disy phi = case phi of
  Top -> []
  Bot -> []
  Var v -> []
  Oneg alpha -> disy alpha
  Oand alpha beta -> disy alpha ++ disy beta
  Oor alpha beta ->  Oor alpha beta: disy alpha ++ disy beta
  Oimp alpha beta -> disy alpha ++ disy beta

numConj :: PL -> Int
numConj phi = case phi of
  Top -> 0
  Bot -> 0
  Var v -> 0
  Oneg alpha -> numConj alpha
  Oand alpha beta -> 1 + numConj alpha + numConj beta
  Oor alpha beta ->  numConj alpha + numConj beta
  Oimp alpha beta -> numConj alpha + numConj beta
