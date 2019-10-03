
-- 1. Estructuras

-- 1.1 Naturales
-- Representacion para los numeros naturales
data Natural = Cero | Suc Natural deriving (Eq, Show)

-- 1.1.1 Dados dos naturales nos dice si el primero es mayor que el segundo
mayorQue :: Natural -> Natural -> Bool
mayorQue (Cero) b = False
mayorQue (Suc a) (Cero) = True
mayorQue (Suc a) (Suc b) = mayorQue a b

-- 1.1.2 Dados dos naturales nos dice si el primero es menor que el segundo
menorQue :: Natural -> Natural -> Bool
menorQue a (Cero) = False
menorQue (Cero) (Suc b) = True
menorQue (Suc a) (Suc b) = menorQue a b

-- 1.1.3 Dados dos naturales nos dice si son iguales
igual :: Natural -> Natural -> Bool
igual (Cero) (Cero) = True
igual (Cero) b = False
igual a (Cero) = False
igual (Suc a) (Suc b) = igual a b

-- 1.2 Lista de Naturales

data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales deriving (Eq, Show)

-- 1.2.1 Dadas dos listas de naturales regresar la concatención de ambas.
concate :: ListaDeNaturales -> ListaDeNaturales -> ListaDeNaturales
concate (Nil) l2 = l2
concate l1 (Nil) = l1
concate (Cons x Nil) l2 = Cons x l2
concate (Cons x xs) l2 = Cons x (concate xs l2)

-- 1.2.2 Dada una lista regresa la reverse de dicha lista.
reversa :: ListaDeNaturales -> ListaDeNaturales
reversa l = case l of
    Nil -> l
    (Cons x Nil) -> l
    (Cons x xs) -> (concate (reversa xs) (Cons x Nil))   