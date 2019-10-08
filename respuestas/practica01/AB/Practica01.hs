module Practica01 where

import Data.List (union)
data Natural = Cero | Suc Natural --deriving(Eq,Show)
instance Eq Natural where
  Cero == Cero = True
  Cero == (Suc _) = False
  (Suc _) == Cero = False
  (Suc a) == (Suc b) =(a == b)

instance Show Natural where
  show (Cero) = "Cero"
  show (Suc n) = "Suc(" ++ show n ++ ")"
{---Dados dos naturales nos dice si el primero es mayor que el segundo. --}
mayorQue:: Natural -> Natural->Bool
mayorQue Cero Cero = False
mayorQue  (Suc _ ) Cero = True
mayorQue Cero (Suc _) = False 
mayorQue (Suc a) (Suc b) = (mayorQue a b) 

{--Dados dos naturales nos dice si el primero es menor que el segundo.--}
menorQue :: Natural ->Natural ->Bool
menorQue Cero Cero = False
menorQue (Suc _) Cero = False
menorQue Cero (Suc _) = True
menorQue (Suc a) (Suc b) = (menorQue a b) 

{-- -Dados dos naturales nos dice si son iguales.-}
igual ::Natural->Natural->Bool
igual Cero Cero = True
igual Cero (Suc _) = False
igual (Suc _) Cero = False
igual (Suc a) (Suc b) = (igual a b) 

{-- Consideremos la siguiente definición de las listas de naturales.--}
data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales

{--Dadas dos listas de naturales regresar la concatenación de ambas. --}
instance Eq ListaDeNaturales where
    Nil == Nil = True
    Nil == (Cons _ _) = False
    (Cons _ _ ) == Nil = False
    (Cons a b) == (Cons c d) = (a==c) && (b==d)
instance Show ListaDeNaturales where
   show Nil = "Nil"
   show (Cons a b) = "(Cons "++show a++" "++show b++")"  

concate :: ListaDeNaturales ->ListaDeNaturales ->ListaDeNaturales
concate Nil a = a
concate a Nil = a
concate (Cons h b) a = (Cons h (concate b a))

{-- Dada una lista regresar la reversa de dicha lista.--}
reversa ::ListaDeNaturales -> ListaDeNaturales
reversa phi = case phi of
   Nil -> Nil
   (Cons a Nil)-> (Cons a Nil)
   (Cons h b) -> concate (reversa b) (Cons h Nil)

{--Consideramos la siguiente representación de la lógica proposicional --}
--Tipo de dato indice
type Indice = Int

--Tipo de dato fórmula
data PL = Top|Bot|Var Indice| Oneg PL |Oand PL PL | Oor PL PL| Oimp PL PL deriving(Eq, Show)

{--Una fórmula regresa un valor de verdad si hay una implicación en
dicha fórmula. --}
hayImplicacion :: PL->Bool
hayImplicacion phi = case phi of
  Top -> False
  Bot ->False
  Var _ -> False
  Oneg p -> hayImplicacion p
  Oor p q -> (hayImplicacion  p) || (hayImplicacion q)
  Oand p q -> (hayImplicacion p) || (hayImplicacion q)
  Oimp p q -> True   

{--Dada una fórmula regresar una lista con las disyunciones de dicha for-
mula.
--}
disy :: PL->[PL]
disy phi = case phi of
  Top -> []
  Bot -> []
  Var _ -> []
  Oneg p -> disy p
  Oor p q -> [Oor p q]`union`(disy p)`union`(disy q)
  Oand p q -> (disy p)`union`(disy q)
  Oimp p q -> (disy p)`union`(disy q)

{--Dada una fórmula regresar el número de conjunciones que tiene dicha
fórmula.--}
numConj :: PL->Int
numConj phi = case phi of
   Top -> 0
   Bot -> 0
   Var _ -> 0
   Oneg p -> (numConj p)
   Oand p q -> 1+(numConj p)+(numConj q)
   Oor p q -> (numConj p)+ (numConj q)
   Oimp p q -> (numConj p)+ (numConj q)
   

