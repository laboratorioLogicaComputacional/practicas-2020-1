module Practica2

where

import SintaxisPL

-- Tipo de datos para modelos
type Modelo = [Indice]

satMod :: Modelo -> PL -> Bool
satMod m phi = case phi of
 Top -> True
 Bot -> False
 Var n -> elem n m
 Oneg alpha -> not(satMod m alpha)
 Oand alpha beta -> (satMod m alpha) && (satMod m beta)
 Oor alpha beta -> (satMod m alpha) || (satMod m beta)
 Oimp alpha beta -> (satMod m (Oneg $ alpha)) || (satMod m beta)

quitaImpl :: PL -> PL
quitaImpl phi = case phi of
  Top -> Top
  Bot -> Bot
  Var n -> Var n
  Oneg alpha -> Oneg $ quitaImpl alpha
  Oand alpha beta -> Oand (quitaImpl alpha) (quitaImpl beta)
  Oor alpha beta -> Oor (quitaImpl alpha) (quitaImpl beta)
  Oimp alpha beta -> Oor (quitaImpl $ Oneg alpha) (quitaImpl beta) 

 -- Función que transforma una fórmula a su forma normal de negación
-- Precondición: no debe tener implicaciones.
noImpNNF :: PL -> PL
noImpNNF phi = case phi of
  -- Casos base:
  Top -> Top
  Bot -> Bot
  Var v -> Var v
  -- Casos recursivos:
  Oneg alfa -> case alfa of
    -- Casos bases (alfa)
    Top -> Bot
    Bot -> Top
    Var v -> Oneg (Var v)
    -- Casos recursivos (alfa)
    Oneg beta -> noImpNNF beta
    Oand beta gamma -> noImpNNF (Oor (Oneg beta) (Oneg gamma))
    Oor beta gamma -> noImpNNF (Oand (Oneg beta) (Oneg gamma))

  Oand alfa beta -> Oand (noImpNNF alfa) (noImpNNF beta)
  Oor alfa beta -> Oor (noImpNNF alfa) (noImpNNF beta)

-- Función que transforma una fórmula a su forma normal de negación.
-- Precondición: ninguna.
toNNF :: PL -> PL
toNNF = noImpNNF . quitaImpl -- Composicion de funciones.


esClausula :: PL -> Bool
esClausula phi = case phi of
  Bot -> True
  Var _ -> True
  Oneg alpha -> case alpha of
    Var _ -> True
    _ -> False
  Oor alpha beta -> esClausula alpha && esClausula beta
  _ -> False


esCNF :: PL -> Bool
esCNF phi = case phi of
  Oand alpha beta -> esCNF alpha && esCNF beta
  _ -> esClausula phi 

esTermino :: PL -> Bool
esTermino phi = case phi of
  Top -> True
  Var _ -> True
  Oneg alpha -> case alpha of
    Var _ -> True
    _ -> False
  Oand alpha beta -> esTermino alpha && esTermino beta
  _ -> False

esDNF :: PL -> Bool
esDNF phi = case phi of
  Oor alpha beta -> esDNF alpha && esDNF beta
  _ -> esTermino phi
