import Data.Array
import Data.List
import System.Random

type Vector a = Array Int a


-- Funcion auxiliar temportal para trabjar con vectores de forma más sencilla (borrar cuando vayamos a entregar)
listaVector :: Num a => [a] -> Vector a
listaVector xs = array (1,length xs) [(y,x) | (x,y) <- (zip xs [1..length xs])]

-- Vectores de prueba (borrar cuando vayamos a entregar)

v1 = listaVector [0.0,2.0]
v2 = listaVector [2.0,2.0]
v3 = listaVector [2.0,0.0]
v4 = listaVector [6.0,5.0]
v5 = listaVector [7.0,2.0]

m1 = listaVector [0.0,0.0]
m2 = listaVector [3.0,3.0]

ms = [m1, m2]
vs = [v1, v2, v3, v4, v5]

vms = asocXM vs ms

-- Distancia Euclidea 
distEuclid :: Floating a => Vector a -> Vector a -> a
distEuclid v1 v2 
    | indices v1 == indices v2 = sqrt (sum[(x - y)**2 | (x,y) <- zip (elems v1) (elems v2)])
    | otherwise = error "Vectores incompatibles"

-- Distancia Manhattan
distManh :: Floating a => Vector a -> Vector a -> a
distManh v1 v2 
    | indices v1 == indices v2 = sum[abs (x - y) | (x,y) <- zip (elems v1) (elems v2)]
    | otherwise = error "Vectores incompatibles"

-- Distancia Hamming
distHamming :: (Floating a,Eq a) => Vector a -> Vector a -> a
distHamming v1 v2 
    | indices v1 == indices v2 = sum[if x == y then 0 else 1 | (x,y) <- zip (elems v1) (elems v2)]
    | otherwise = error "Vectores incompatibles"

-- Generación de centros iniciales aleatorios
-- centrosIniciales :: Int -> Float -> Float -> Float -> Float -> IO [(Float,Float)]
-- centrosIniciales n minx maxx miny maxy = do
--   gen <- newStdGen 
--   gen2 <- newStdGen  
--   let xs = randomRs (minx, maxx) gen
--   let ys = randomRs (miny, maxy) gen2
--       (as, _) = splitAt n xs
--       (bs, _)  = splitAt n ys
--   return (zip as bs)

-- generaUnNumero :: Int -> [(Float,Float)] -> IO [Float]
-- generaUnNumero i xs
--     | i == (length xs) = return [] 
--     | otherwise =  do
--         gen <- newStdGen
--         let (randNumber, newGen) = randomR (xs!!i) gen :: (Float, StdGen)
--         let ys = (generaUnNumero (i+1) xs)
--         return (randNumber:ys)

--kMeans :: (Floating a ,Ord a) => Integer -> [Vector a] -> [Vector a]
kMeans k xs = kMeansAux xs m where m = ms --Solo pruebas, hay que añadir funcion que calcule m

--kMeans :: (Floating a ,Ord a) => Integer -> [Vector a] -> [Vector a]
kMeansAux xs m 
    | getNewM xms m == m = m
    | otherwise = kMeansAux xs (getNewM xms m)
        where xms = asocXM xs m

getNewM xms [] = []
getNewM xms (m:ms) = [calculaMediaM xms m 0 (replicate (fromIntegral(length m)) 0)] ++ (getNewM xms ms)

calculaMediaM [] m 0 _ = m
calculaMediaM [] m cont acc = listaVector [(acc!!(i-1)) / (fromIntegral(cont)) | i <- [1..(length m)]]
calculaMediaM (xm:xms) m cont acc = if (snd xm) == m then calculaMediaM xms m (cont+1) [i + j  | (i,j) <- zip (elems (fst xm)) (acc)] else calculaMediaM xms m cont acc

asocXM xs ms 
    | null xs || null ms = error "Lista de vectores o centros vacia"
    | otherwise = asocXMAux xs ms []

asocXMAux :: (Floating a,Ord a) => [Vector a] -> [Vector a] -> [(Vector a,Vector a)] -> [(Vector a,Vector a)]
asocXMAux [] _ acc = acc
asocXMAux (x:xs) ms acc = asocXMAux xs ms ([(x,(getMinDist x ms))] ++ acc)


getMinDist x ms = snd (head (sortBy fstTuple [((distEuclid x m), m) | m <- ms]))  --Cambiar para que se le pueda pasar la distancia como argumento

fstTuple (x1,y1) (x2,y2)
    | x1 > x2 = GT 
    | otherwise = LT