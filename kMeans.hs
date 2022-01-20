module KMeans 
    ( kMeans,
      kMeansCompleto,
      asocXM
    ) where 

import Distancias
import Data.Array
import Data.List
import System.Random
import CentrosAleatorios

-- Vectores de prueba (borrar cuando vayamos a entregar)

-- v1 = listaVector [0.0,2.0]
-- v2 = listaVector [2.0,2.0]
-- v3 = listaVector [2.0,0.0]
-- v4 = listaVector [-1.0,2.0]
-- v5 = listaVector [3.0,-1.0]
-- v6 = listaVector [0.0,1.0]
-- v7 = listaVector [1.0,-2.0]
-- v8 = listaVector [-1.0,-2.0]

-- m1 = listaVector [0.0,0.0]
-- m2 = listaVector [3.0,3.0]
-- m3 = listaVector [-1.0,3-1.0]

-- ms = [m1, m2, m3]
-- vs = [v1, v2, v3, v4, v5, v6, v7, v8]

-- vms = asocXM vs ms


kMeans :: Int -> [Vector] -> Distancia -> IO [Vector]
kMeans k xs d = do
    m <- generaCentros k xs
    return (kMeansAux xs m d) --Solo pruebas, hay que añadir funcion que calcule m

kMeansCompleto :: Int -> [Vector] -> Distancia -> IO [(Vector,Vector)]
kMeansCompleto k xs d = do
    m <- generaCentros k xs 
    let mFinal = kMeansAux xs m d
    return (asocXM xs mFinal d)

kMeansAux :: [Vector] -> [Vector] -> Distancia -> [Vector]
kMeansAux xs m d
    | getNewM xms m == m = m
    | otherwise = kMeansAux xs (getNewM xms m) d
        where xms = asocXM xs m d

getNewM xms [] = []
getNewM xms (m:ms) = [calculaMediaM xms m] ++ (getNewM xms ms) --Quizas hacer una auxiliar mas para no tener que pasar el replicate como argumento

calculaMediaM xms m = calculaMediaMAux xms m 0 (replicate (fromIntegral(length m)) 0)

calculaMediaMAux [] m 0 _ = m
calculaMediaMAux [] m cont acc = listaVector [(acc!!(i-1)) / (fromIntegral(cont)) | i <- [1..(length m)]]
calculaMediaMAux (xm:xms) m cont acc = if (snd xm) == m then calculaMediaMAux xms m (cont+1) [i + j  | (i,j) <- zip (elems (fst xm)) (acc)] else calculaMediaMAux xms m cont acc

asocXM :: [Vector] -> [Vector] -> Distancia -> [(Vector,Vector)]
asocXM xs ms d
    | null xs || null ms = error "Lista de vectores o centros vacia"
    | otherwise = asocXMAux xs ms d []

--asocXMAux :: [Vector ] -> [Vector ] -> [(Vector ,Vector )] -> [(Vector ,Vector )]
asocXMAux [] _ d acc = acc
asocXMAux (x:xs) ms d acc = asocXMAux xs ms d ([(x,(getMinDist x ms d))] ++ acc)


getMinDist x ms d = snd (head (sortBy fstTuple [((d x m), m) | m <- ms]))  --Cambiar para que se le pueda pasar la distancia como argumento

fstTuple (x1,y1) (x2,y2)
    | x1 > x2 = GT 
    | otherwise = LT
