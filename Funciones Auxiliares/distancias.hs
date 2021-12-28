import Data.Array
import System.Random

type Vector a = Array Int a


-- Funcion auxiliar temportal para trabjar con vectores de forma más sencilla (borrar cuando vayamos a entregar)
listaVector :: Num a => [a] -> Vector a
listaVector xs = array (1,length xs) [(y,x) | (x,y) <- (zip xs [1..length xs])]

-- Vectores de prueba (borrar cuando vayamos a entregar)

v1 = listaVector [1.0,2.0,3.0,4.0]
v2 = listaVector [0.0,1.0,0.0,1.0]
v3 = listaVector [3.0,7.0,3.0,6.0]
v4 = listaVector [1.0,1.0,1.0,1.0,1.0]

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