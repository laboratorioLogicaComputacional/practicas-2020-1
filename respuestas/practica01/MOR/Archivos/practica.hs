module Practica1 where

-- Práctica 1

    -- Naturales --

    data Natural = Cero | Suc Natural deriving(Eq,Show)

    {- Función que determina si una número es mayor a otro -}
    mayorQue :: Natural -> Natural -> Bool
    mayorQue Cero Cero       = False
    mayorQue _ Cero          = True
    mayorQue Cero _          = False
    mayorQue (Suc n) (Suc m) = mayorQue n m

    {- Función que determina si una número es menor a otro -}
    menorQue :: Natural -> Natural -> Bool
    menorQue Cero Cero       = False
    menorQue _ Cero          = False
    menorQue Cero _          = True
    menorQue (Suc n) (Suc m) = menorQue n m

    {- Función que determina si dos números son iguales -}
    igual :: Natural -> Natural -> Bool
    igual Cero Cero       = True
    igual Cero _          = False
    igual _ Cero          = False
    igual (Suc n) (Suc m) = igual n m


    data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales deriving(Show)

    {- Función que concatena dos listas de naturales -}
    concate :: ListaDeNaturales -> ListaDeNaturales -> ListaDeNaturales
    concate l Nil          = l
    concate Nil l          = l
    concate (Cons n l1) l2 = Cons n (concate l1 l2)

    {- Función que devuelve la reversa de una lista de enteros -}
    reversa :: ListaDeNaturales -> ListaDeNaturales
    reversa Nil                 = Nil
    reversa (Cons n Nil)        = Cons n Nil
    reversa (Cons n (Cons m l)) = concate (reversa (Cons m l)) (Cons n Nil)

    -- Lógica Proposicional --

    data PL = Top | Bot | Var Indice
              | Oneg PL
              | Oand PL PL | Oor PL PL
              | Oimp PL PL deriving(Eq,Show)

    type Indice = Int

    {- Función que verifica si en una fórmula h+existe alguna implicación-}
    hayImplicacion :: PL -> Bool
    hayImplicacion e = case e of 
        Top        -> False
        Bot        -> False
        (Var n)    -> False
        (Oneg p)   -> hayImplicacion p
        (Oand p q) -> (hayImplicacion p) || (hayImplicacion q)
        (Oor p q)  -> (hayImplicacion p) || (hayImplicacion q)
        (Oimp p q) -> True

    {- Función que verifica si hay disyunciones en una fórmula -}
    disy :: PL -> [PL]
    disy e = case e of
        Top        -> []
        Bot        -> []
        (Var n)    -> []
        (Oneg p)   -> disy p
        (Oand p q) -> eliminaR ((disy p) ++ (disy q))
        (Oor p q)  -> [Oor p q] ++ eliminaR ((disy p) ++ (disy q))
        (Oimp p q) -> eliminaR ((disy p) ++ (disy q))

    {- Función que determina el número de conjunciones en una fórmula -}
    numConj :: PL -> Int
    numConj e = case e of
        Top        -> 0
        Bot        -> 0
        (Var n)    -> 0
        (Oneg p)   -> numConj p
        (Oand p q) -> 1 + (numConj p) + (numConj q)
        (Oor p q)  -> (numConj p) + (numConj q)
        (Oimp p q) -> (numConj p) + (numConj q)

    
