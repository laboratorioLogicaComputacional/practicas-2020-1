module Practica1
where

data Natural = Cero | Suc Natural deriving (Eq, Show)

data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales deriving (Eq, Show)

mayorQue :: Natural -> Natural -> Bool
mayorQue n m = case n of
  Cero -> False
  Suc a -> case m of
    Cero -> True
    Suc b -> mayorQue a b

menorQue :: Natural -> Natural -> Bool
menorQue n m = case m of
  Cero -> False
  Suc b -> case n of
    Cero -> True
    Suc a -> menorQue a b

igual :: Natural -> Natural -> Bool
igual n m = case n of
  Cero -> case m of
    Cero -> True
    Suc _ -> False
  Suc a -> case m of
    Cero -> False
    Suc b -> igual a b

concate :: ListaDeNaturales -> ListaDeNaturales -> ListaDeNaturales
concate l1 l2 = case l1 of
 Nil -> l2
 Cons n l -> Cons n (concate l l2)

reversa :: ListaDeNaturales -> ListaDeNaturales
reversa l = case l of
 Nil -> Nil
 Cons n ls -> concate (reversa ls) (Cons n Nil)


-- Tipo de dato indice
type Indice = Int

-- Tipo de dato fórmula
data PL = Top | Bot  | Var Indice
              | Oneg PL 
              | Oand PL PL | Oor PL PL 
              | Oimp PL PL deriving (Eq, Show)

disy :: PL -> [PL]
disy phi = case phi of
  Top -> []
  Bot -> []
  Var _ -> []
  Oneg alpha -> conj phi
  Oand alpha beta -> conj alpha ++ conj beta
  Oor alpha beta -> [Oor alpha beta] ++ conj alpha ++ conj beta
  Oimp alpha beta -> conj alpha ++ conj beta

numConj :: PL -> Int
numConj phi = case phi of
  Top -> 0
  Bot -> 0
  Var _ -> 0
  Oneg alpha -> numConj alpha
  Oand alpha beta -> 1 + numConj alpha + numConj beta
  Oor alpha beta -> numConj alpha + numConj beta
  Oimp alpha beta -> numConj alpha + numConj beta

