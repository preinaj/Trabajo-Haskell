import Data.Array
import Data.List
import System.Random

type Vector a = Array Int a

distEuclid :: Floating a => Vector a -> Vector a -> a
distEuclid v1 v2 
    | indices v1 == indices v2 = sqrt (sum[(x - y)**2 | (x,y) <- zip (elems v1) (elems v2)])
    | otherwise = error "Vectores incompatibles"

v1 = listaVector [0.0,2.0]
v2 = listaVector [2.0,2.0]
v3 = listaVector [2.0,0.0]
v4 = listaVector [6.0,5.0]
v5 = listaVector [7.0,2.0]

m1 = listaVector [0.0,0.0]
m2 = listaVector [3.0,3.0]

ms = [m1, m2]
vs = [v1, v2, v3, v4, v5]

ms' = [[m1], [m2]]
vs' = [[v1], [v2], [v3], [v4], [v5]]

-- Funcion auxiliar temportal para trabjar con vectores de forma más sencilla (borrar cuando vayamos a entregar)
listaVector :: Num a => [a] -> Vector a
listaVector xs = array (1,length xs) [(y,x) | (x,y) <- (zip xs [1..length xs])]


clusterAglomerativoAux vss 
    | length vss /= 1 =  xy:(eliminaElem y (eliminaElem x vss))
    | otherwise = []
        where (x,y) = fst (distanciaMinima vss)
              xy = x ++ y



distanciaMinima vss = head(sortBy sndTuple (calculaMatrixProximidad vs'))

calculaMatrixProximidad :: (Floating a, Ord a) => [[Vector a]] -> [(([Vector a], [Vector a]), a)]
calculaMatrixProximidad [] = []
calculaMatrixProximidad (vs:vss) = calculaDistanciasAUnCluster vs vss ++ (calculaMatrixProximidad vss)

calculaDistanciasAUnCluster vs [] = []
calculaDistanciasAUnCluster vs (xs:xss) = ((vs,xs), distanciaEntreClusters vs xs):(calculaDistanciasAUnCluster vs xss)


distanciaEntreClusters :: (Floating a, Ord a) => [Vector a] -> [Vector a] -> a
distanciaEntreClusters v1 v2 = distEuclid vm1 vm2
    where vm1 = calculaMedia v1
          vm2 = calculaMedia v2


calculaMedia v = calculaMediaAux v 0 (replicate (fromIntegral (length (elems (v!!0)))) 0)

calculaMediaAux [] cont acc = listaVector [(acc!!(i-1)) / (fromIntegral(cont)) | i <- [1..(length acc)]]
calculaMediaAux (xm:xms) cont acc = calculaMediaAux xms (cont+1) [i + j  | (i,j) <- zip (elems xm) (acc)] 

sndTuple (x1,y1) (x2,y2)
    | y1 > y2 = GT 
    | otherwise = LT


eliminaElem _ [] = []
eliminaElem x (y:ys) 
    | x == y    = ys
    | otherwise = y : eliminaElem x ys