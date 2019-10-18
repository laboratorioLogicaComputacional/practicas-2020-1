module Practica2

where

import SintaxisPL


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
