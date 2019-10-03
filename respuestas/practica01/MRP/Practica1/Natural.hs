module Natural
  where

data Natural = Cero | Suc Natural deriving (Eq, Show)
data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales deriving (Eq, Show)

-- | Dados dos naturales, determina si el primero es mayor que el segundo.

mayorQue :: Natural -> Natural -> Bool
mayorQue n m = case n of
  Cero -> False
  Suc o -> case m of
    Cero -> True
    Suc p -> mayorQue o p
 
-- | Dados dos naturales, determina si el primero es mayor que el segundo.

menorQue :: Natural -> Natural -> Bool
menorQue n m = case m of
  Cero -> False
  Suc p -> case n of
    Cero -> True
    Suc o -> menorQue o p

-- | Dados dos naturales, determina si son iguales.

igual :: Natural -> Natural -> Bool
igual n m = case n of
  Cero -> case m of
    Cero -> True
    Suc _ -> False
  Suc o -> case m of
    Cero -> False
    Suc p -> igual o p

-- | Dadas dos listas de naturales, regresa la concatenacion de ambas.

concate :: ListaDeNaturales -> ListaDeNaturales -> ListaDeNaturales
concate lis li = case lis of
  Nil -> li
  Cons n l -> Cons n (concate l li)
  

-- | Dada una lista regresa la reversa de dicha lista.

reversa :: ListaDeNaturales -> ListaDeNaturales
reversa list = case list of
  Nil -> Nil
  Cons n lis -> concate (reversa lis) (Cons n Nil)
