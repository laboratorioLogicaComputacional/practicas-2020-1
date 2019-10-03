{-Facultad de Ciencias UNAM - Lógica Computacional 2020-1 
		  Profesor: Dr. Miguel Carrillo Barajas 
		  Ayudante: Sara Doris Montes Incin
		  Laboratorio: Mauricio Esquivel Reyes-}

module DeduccionL
  where

import SintaxisPLI

-- Función que nos dice si una formula de PLI cumple el Axioma 1
esAxL1 :: PLI -> Bool
esAxL1 phi = case phi of
  Bot -> False
  Var i -> False
  Oimp alpha beta -> esAxL1Aux alpha beta

-- Funcion auxiliar que verifica si la primera formula es igual a la implicación de la segunda.
esAxL1Aux :: PLI -> PLI -> Bool  
esAxL1Aux alpha phi = case phi of 
  Bot -> False 
  Var i -> False
  Oimp beta gamma -> alpha == gamma 

-- Función que nos dice si una formula de PLI cumple el Axioma 2
esAxL2 :: PLI -> Bool
esAxL2 phi = case phi of
  Bot -> False
  Var i -> False
  Oimp alpha beta -> (esAxL2Aux alpha beta)

-- Función auxiliar para esAxl2 que nos dice si una formula cumple el Axioma 2.
-- verificando que la primera formula sea una implicación.
esAxL2Aux :: PLI -> PLI -> Bool
esAxL2Aux phi b = case phi of
  Bot -> False
  Var i -> False
  Oimp alpha  beta -> (if (esAxL2Aux1 beta) then (compara phi b) else False)

-- Función que nos dice si los elementos de la primera formula son iguales a la los elementos de la segunda.
compara :: PLI -> PLI -> Bool
compara a phi = case phi of 
  Bot -> False
  Var i -> False
  Oimp x y -> ((Oimp (regresaAlpha a) (regresaBeta a)) == x &&  (Oimp (regresaAlpha a) (regresaGamma a)) == y)  

-- Verifica que una formula sea una implicación
esAxL2Aux1 :: PLI -> Bool
esAxL2Aux1 phi = case phi of
  Bot -> False
  Var i -> False
  Oimp alpha  beta -> True 

-- Regresa el antecedente de la implicación.
regresaAlpha :: PLI -> PLI
regresaAlpha (Oimp alpha beta) =  alpha
regresaAlpha _ = error "La formula no es una implicación"

-- Regresa el consecuente de la implicación
regresaB :: PLI -> PLI
regresaB (Oimp alpha beta) =  beta  
regresaB _ = error "La formula no es una implicación"

-- Regresa el antecedente del consecuente de la implicación 
regresaBeta :: PLI -> PLI
regresaBeta (Oimp alpha beta) = (regresaAlpha beta)
regresaBeta _ = error "La formula no es una implicación"

-- Regresa el consecuente del consecuente de la implicación 
regresaGamma :: PLI -> PLI
regresaGamma (Oimp alpha beta) = (regresaB beta)
regresaGamma _ = error "La formula no es una implicación"  


-- Función que nos dice si una formula de PLI cumple el Axioma 3
esAxL3 :: PLI -> Bool
esAxL3 phi = case phi of
  Bot -> False
  Var i -> False
  Oimp alpha beta -> (esAxL3Aux alpha beta)

-- Función auxiliar que nos dice si una formula de PLI cumple el Axioma 3
esAxL3Aux :: PLI -> PLI -> Bool
esAxL3Aux phi b = case phi of
  Bot -> False
  Var i -> False
  Oimp alpha beta -> (if ((negacion alpha) && (negacion beta)) 
    then ((Oimp (regresaAlpha beta) (regresaAlpha alpha)) == b) else False)

-- verifica que la formula sea una negación
negacion :: PLI -> Bool
negacion phi = case phi of 
  Bot -> False
  Var i -> False
  Oimp alpha beta -> (if (beta == Bot) then True else False) 

-- Función que nos dice si una formula es una Axioma del sistema L
esAxiomaDeL :: PLI -> Bool
esAxiomaDeL phi = (esAxL1 phi) || (esAxL2 phi) || (esAxL3 phi)

-- Función que nos dice si se aplico de manera correcta Modus Ponens
esModusPonens :: (PLI, PLI, PLI) -> Bool
esModusPonens (phi, chi, psi) = case (phi, chi, psi) of
  (_, Bot , _) -> False
  (_, Var i, _) -> False
  (phi, Oimp alpha beta, psi) -> (phi == alpha && psi == beta)

-- Reglas del sistema L
data ReglaL = Prem           -- Las premisas son validas
            | Ax             -- Las formulas pueden ser axiomas
            | ModPon Int Int -- Es resultado de aplicar MP a dos formulas anteriores
            deriving (Eq,Show)
-- Tipo Paso
-- Una fomula PLI y la Regla utilizada
type Paso = (PLI, ReglaL)

-- Tipo Numero de Paso
type NumPaso = (Int, Paso)

-- Nos regresa la formula del paso n
phiPasoNum :: Int -> [NumPaso] -> PLI
phiPasoNum i lpasos = case mpi of
  Just (phi, _) -> phi
  _ -> error $ "phiPasoNum: indice fuera de rango."
  where
    mpi = lookup i lpasos
    
-- Nos regresa el último NumPaso de una lista
ultimoPaso :: [NumPaso] -> NumPaso
ultimoPaso lpasos
  | lpasos /= [] = (n,p)
  | otherwise = (0,(oTop,Prem))
  where
    (n,p) = last lpasos

-- Revisa que el paso sea correcto
checkPaso :: [PLI] -> [NumPaso] -> NumPaso -> Bool
checkPaso lprem lpp p = case p of
  (n, (phi, Prem)) -> (esPremisa phi lprem)-- Revisamos que phi sea parte de lprem
  (n, (phi, Ax)) -> (esAxiomaDeL phi) -- Revisamos que phi sea un axioma
  (n, (phi, ModPon i j)) -> esModusPonens (alpha, beta, phi) && n == nU+1 -- Revisamos que phi sea resultado de hacer modus ponens con i y j
    where
      alpha = phiPasoNum i lpp
      beta = phiPasoNum j lpp
  where
    (nU,(fU,_)) = ultimoPaso lpp

esPremisa :: PLI -> [PLI] -> Bool
esPremisa phi [] = False
esPremisa phi (x:xs)
  | phi == x = True
  | otherwise = esPremisa phi xs    

-- Revisa que la prueba sea correcta
checkPrueba :: [PLI] -> [NumPaso] -> Bool
checkPrueba lprem lpasos = case lpasos of
  []      -> True -- Si la lista es vacía entonces es cierto
  _:_     -> checkPrueba lprem lpp && checkPaso lprem lpp p -- Hacemos recursión.
  where
    n = length lpasos
    lpp = Prelude.take (n-1) lpasos
    p = last lpasos

--Muestra una lista de formulas.
showLphi :: [PLI] -> String
showLphi lphi= case lphi of
                    [f]     -> showPLI f
                    f:lf    -> showPLI f ++","++ showLphi lf
                    []      -> ""

-- Muesta la regla
showRegla :: ReglaL -> String
showRegla r = case r of
  Prem -> "premisa"
  Ax -> "axioma"
  ModPon i j -> "Modus Ponens "++show i++","++show j

-- Muestra un paso indicando, mediante b, si es correcto, o no.
showNumPasoCheck :: Int -> NumPaso -> Bool -> String
showNumPasoCheck fSize (n,(phi, r)) b = "\t" ++ (show n) ++ ", " ++ fS ++ spaceAfterPhi ++ rS ++ checks
  where
    fS = showPLI phi
    spaceAfterPhi = " " ++ Prelude.take (fSize-(length fS)) (repeat ' ')
    rS = "\t" ++ (showRegla r)
    checks = if b
      then ". Correcto"
      else ". Incorrecto"

-- Muestra los pasos de lpasos indicando si son correctos, o no.
-- Initial call: showLpasos fSize lprem [] lpasos
showLpasos :: Int -> [PLI] -> [NumPaso] -> [NumPaso] -> IO ()
showLpasos fSize lprem prevLp lpasos = case lpasos of
  [] -> putStr ""
  p:lps -> do
    putStrLn $ showNumPasoCheck fSize p (checkPaso lprem prevLp p)
    showLpasos fSize lprem (prevLp++[p]) lps

-- Muestra el resultado de la prueba realizada.
showCheckConclusion :: [PLI] -> [NumPaso] -> PLI -> IO ()
showCheckConclusion lpremisas lpasos phi =
  do
    putStrLn mensaje
    putStrLn ""
    where
      mensaje
        | not pruebaOK = "\t*** Hay pasos incorrectos. ***"
        | phi /= fN = "\t*** La ultima formula no es el objetivo: ***"++ (showPLI phi) ++" /= "++ (showPLI fN)
        | otherwise =  "\tCorrecto. Mediante el sistema L: "++ lpremS ++ " |- " ++ showPLI fN
      pruebaOK = checkPrueba lpremisas lpasos
      (_,(fN,_)) = ultimoPaso lpasos
      lpremS = if lpremisas /= []
        then "{" ++ showLphi lpremisas ++"}"
        else ""
        
-- Función que nos regresa el elemento más grande.
maxL :: Ord a => [a] -> a
maxL = foldr1 (\x y ->if x >= y then x else y)

-- Revisa si los pasoso son correctos y el resultado de la prueba realizada.
esDeduccionEnL :: [PLI] -> [NumPaso] -> PLI -> IO()
esDeduccionEnL lpremisas lpasos phi =
  do
    showLpasos fSize lpremisas [] lpasos
    showCheckConclusion lpremisas lpasos phi
  where
    fSize = maxL [length (showPLI f) | (_,(f,_)) <- lpasos ]
