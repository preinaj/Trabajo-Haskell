module CentrosAleatorios 
    (generaCentros
    ) where 

import Data.Array
import Data.List
import System.Random
import Distancias 
import Data.Typeable

listaMinMax :: [Vector] -> [(Double, Double)]
listaMinMax lvectores
    | null lvectores = error "No se pueden inicializar los centros a partir de un dataset vacio"
    | otherwise = foldr(\coord rr -> (minMaxCoord coord lvectores) : rr) [] lcoord
        where   dimension = (snd.bounds.head) lvectores
                lcoord = [1..dimension]

minMaxCoord :: Int -> [Vector] -> (Double, Double)
minMaxCoord coord lvectores = (minimum lcoord, maximum lcoord)
    where lcoord = extraeValoresCoord coord lvectores

extraeValoresCoord :: Int -> [Vector] -> [Double]
extraeValoresCoord coord lvectores = map(\v -> v!coord) lvectores

-----------------------

listaVectorIO :: IO[Double] -> IO Vector
listaVectorIO xs = do
    xs' <- xs
    return  (array (1,length xs') [(y,x) | (x,y) <- (zip xs' [1..length xs'])])

generaCentros :: Int -> [Vector] -> IO [Vector]
generaCentros k lvectores
    | k == 0 = return []
    | otherwise = do 
        let listaMinMax2 = listaMinMax lvectores -- [(0,0.5),(0,0.5)]
        x <- listaVectorIO (generaUnNumero 0 listaMinMax2)
        ys <- (generaCentros (k-1) lvectores)
        return ([x] ++ ys)

generaUnNumero :: Int -> [(Double,Double)] -> IO [Double]
generaUnNumero i xs
    | i == (length xs) = return [] 
    | otherwise =  do
        gen <- newStdGen
        let (randNumber, newGen) = randomR (xs!!i) gen :: (Double, StdGen)
        ys <- (generaUnNumero (i+1) xs)
        return (randNumber:ys)