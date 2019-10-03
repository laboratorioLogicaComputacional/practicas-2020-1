 {-
 -- Práctica 1
 -- Cárdenas Torres Ernesto
 -}

{- 1. Estructuras -} 

{- 1.1 Naturales -}

data Natural = Cero | Suc Natural deriving(Eq, Show)

{- Función 1
-- Recibe dos naturales a y b para devolver el resultado
-- de a > b
-}

mayorQue :: Natural -> Natural -> Bool
mayorQue Cero Cero = False
mayorQue Cero (Suc a) = False
mayorQue (Suc a) Cero = True
mayorQue (Suc a) (Suc b) = mayorQue a b

{- Función 2
-- Recibe dos naturales a y b para devolver el resultado
-- de a < b
-}

menorQue :: Natural -> Natural -> Bool
menorQue Cero Cero = False
menorQue Cero (Suc a) = True
menorQue (Suc a) Cero = False
menorQue (Suc a) (Suc b) = menorQue a b

{- Función 3
-- Recibe dos naturales a y b para devolver el resultado
-- de a = b
-}

igual :: Natural -> Natural -> Bool
igual Cero Cero = True
igual (Suc a) Cero = False
igual Cero (Suc b) = False
igual (Suc a) (Suc b) = igual a b


{- 1.2 Lista de Naturales -}

data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales deriving(Eq, Show)

{- Función 1
-- Recibe dos ListaDeNaturales y devuelve la concatenación de las mismas
-}

concate :: ListaDeNaturales -> ListaDeNaturales -> ListaDeNaturales
concate Nil Nil = Nil
concate Nil a = a
concate a Nil = a
concate (Cons a b) c = Cons a (concate b c)

{- Función 2
-- Recibe una ListaDeNaturales y devuelve su reversa
-}

reversa :: ListaDeNaturales -> ListaDeNaturales
reversa Nil = Nil
reversa (Cons a Nil) = (Cons a Nil)
reversa (Cons a b) = reversaAux (Cons a b) Nil



{- 2. Lógica Proposicional -}

-- Tipo de dato indice
type Indice = Int

-- Tipo de dato fórmula
data PL = Top | Bot | Var Indice| Oneg PL| Oand PL PL | Oor PL PL
        | Oimp PL PL deriving (Eq, Show)

{- 2.1 Función que nos dice si una fórmula tiene implicaciones -}

hayImplicacion :: PL -> Bool
hayImplicacion phi = case phi of
    Top -> False
    Bot -> False
    Var x -> False
    Oneg alpha -> hayImplicacion alpha
    Oand alpha beta -> hayImplicacion alpha || hayImplicacion beta
    Oor alpha beta -> hayImplicacion alpha || hayImplicacion beta
    Oimp alpha beta -> True

{- 2.2 Función que nos regresa una lista con las disyunciones de cierta fórmula -}
disy :: PL -> [PL]
disy phi = case phi of
    Top -> []
    Bot -> []
    Var x -> []
    Oneg alpha -> disy alpha
    Oand alpha beta -> disy alpha ++ disy beta
    Oor alpha beta -> [Oor alpha beta] ++ disy alpha ++ disy beta
    Oimp alpha beta -> disy alpha ++ disy beta

{- 2.3 Función que recibe una fórmula de la lógica proposicional
-- y regresa el número de conjunciones -}
numConj :: PL -> Int
numConj phi = case phi of
  Top -> 0
  Bot -> 0
  Var x -> 0
  Oneg alpha -> numConj alpha
  Oand alpha beta -> 1 + numConj alpha + numConj beta
  Oor alpha beta -> numConj alpha + numConj beta
  Oimp alpha beta -> numConj alpha + numConj beta

{- Funciones Auxiliares -}

{- Recibe dos listas y simula una pila -}

reversaAux :: ListaDeNaturales -> ListaDeNaturales -> ListaDeNaturales
reversaAux Nil Nil = Nil
reversaAux Nil e = e
reversaAux (Cons a b) e = reversaAux b (Cons a e)