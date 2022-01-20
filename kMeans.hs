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


--kMeans :: (Floating a ,Ord a) => Integer -> [Vector a] -> [Vector a]
kMeans k xs = do
    m <- generaCentros k xs
    return (kMeansAux xs m) --Solo pruebas, hay que añadir funcion que calcule m

kMeansCompleto k xs = do
    m <- generaCentros k xs 
    let mFinal = kMeansAux xs m
    return (asocXM xs mFinal)

--kMeans :: (Floating a ,Ord a) => Integer -> [Vector a] -> [Vector a]
kMeansAux xs m 
    | getNewM xms m == m = m
    | otherwise = kMeansAux xs (getNewM xms m)
        where xms = asocXM xs m

getNewM xms [] = []
getNewM xms (m:ms) = [calculaMediaM xms m] ++ (getNewM xms ms) --Quizas hacer una auxiliar mas para no tener que pasar el replicate como argumento

calculaMediaM xms m = calculaMediaMAux xms m 0 (replicate (fromIntegral(length m)) 0)

calculaMediaMAux [] m 0 _ = m
calculaMediaMAux [] m cont acc = listaVector [(acc!!(i-1)) / (fromIntegral(cont)) | i <- [1..(length m)]]
calculaMediaMAux (xm:xms) m cont acc = if (snd xm) == m then calculaMediaMAux xms m (cont+1) [i + j  | (i,j) <- zip (elems (fst xm)) (acc)] else calculaMediaMAux xms m cont acc

asocXM xs ms 
    | null xs || null ms = error "Lista de vectores o centros vacia"
    | otherwise = asocXMAux xs ms []

asocXMAux :: [Vector ] -> [Vector ] -> [(Vector ,Vector )] -> [(Vector ,Vector )]
asocXMAux [] _ acc = acc
asocXMAux (x:xs) ms acc = asocXMAux xs ms ([(x,(getMinDist x ms))] ++ acc)


getMinDist x ms = snd (head (sortBy fstTuple [((distEuclidea x m), m) | m <- ms]))  --Cambiar para que se le pueda pasar la distancia como argumento

fstTuple (x1,y1) (x2,y2)
    | x1 > x2 = GT 
    | otherwise = LT
