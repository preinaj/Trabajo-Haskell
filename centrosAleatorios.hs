module CentrosAleatorios 
    (generaCentros
    ) where 

import Data.Array
import Data.List
import System.Random

type Vector = Array Int Double

listaVector :: IO[Double] -> IO Vector
listaVector xs = do
    xs' <- xs
    return  (array (1,length xs') [(y,x) | (x,y) <- (zip xs' [1..length xs'])])

listaMinMax = [(0,0.5),(0,0.5)]   --Esto hay que sacarlo del maximo del csv. Es una lista de tuplas con el maximo y minimo de cada coordenada

generaCentros :: Int -> IO [Vector]
generaCentros k
    | k == 0 = return []
    | otherwise = do 
        x <- listaVector (generaUnNumero 0 listaMinMax)
        ys <- (generaCentros (k-1))
        return ([x] ++ ys)

generaUnNumero :: Int -> [(Double,Double)] -> IO [Double]
generaUnNumero i xs
    | i == (length xs) = return [] 
    | otherwise =  do
        gen <- newStdGen
        let (randNumber, newGen) = randomR (xs!!i) gen :: (Double, StdGen)
        ys <- (generaUnNumero (i+1) xs)
        return (randNumber:ys)