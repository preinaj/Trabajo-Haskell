import Data.Array
import Data.List
import System.Random

-- Codigo para probarlo
-- > d = inicializaClusteringAglomerativo lv
-- > clusteringAglomerativo d


type Vector = Array Int Double

type Cluster = [Vector]
type Nivel = (Int, [Cluster], Int) -- (Numero de nivel, lista de clusters de ese nivel, numero de clusters de ese nivel)
type EvolucionClusters = [Nivel]


v1 = listaVector [2.0,0.0]
v2 = listaVector [10.0,20.0]
v3 = listaVector [2.0,1.0]
v4 = listaVector [15.0,7.0]
--v5 = listaVector [7.0,2.0]

-- m1 = listaVector [0.0,0.0]
-- m2 = listaVector [3.0,3.0]

-- ms = [m1, m2]
-- vs = [v1, v2, v3, v4, v5]

-- ms' = [[m1], [m2]]
-- vs' = [[v1], [v2], [v3], [v4], [v5]]

lv = [v1, v2, v3, v4]

-- Funcion auxiliar temportal para trabajar con vectores de forma más sencilla (borrar cuando vayamos a entregar)
listaVector :: [Double] -> Vector
listaVector xs = array (1,length xs) [(y,x) | (x,y) <- (zip xs [1..length xs])]

-------------------------------

-- Obtiene el primer nivel a partir de los datos:
-- todos los elementos forman un cluster por si mismos
inicializaClusteringAglomerativo :: [Vector] -> EvolucionClusters
inicializaClusteringAglomerativo puntosIniciales = [ ( 0, [ [punto] | punto <- puntosIniciales], length puntosIniciales ) ]

-- Funcion base del algoritmo de clustering: obtiene la evolucion de la lista de clusters
clusteringAglomerativo :: EvolucionClusters -> EvolucionClusters
clusteringAglomerativo listaEvolucion 
    | condParada        = listaEvolucion
    | otherwise         = clusteringAglomerativo (listaEvolucion ++ [ calculaSiguienteNivel nivelActual ] )
    where   nivelActual@(numNivel, listclusters, numClusters) = last listaEvolucion
            condParada = length listclusters == 1

-- Dado un nivel, toma la correspondiente lista de clusters, fusiona los dos clusters mas cercanos y devuelve el siguiente nivel
calculaSiguienteNivel :: Nivel -> Nivel
calculaSiguienteNivel nivelActual = ( numNivel+1, listClustersNueva, length listClustersNueva )
    where   (numNivel, listclusters, numClusters) = nivelActual
            clustersMasCercanos@(c1,c2) = fst $ clustersDistanciaMinima listclusters
            fusionClustersMasCercanos = c1 ++ c2
            listClustersNueva =  fusionClustersMasCercanos : (eliminaCluster c2 (eliminaCluster c1 listclusters))

eliminaCluster :: Cluster -> [Cluster] -> [Cluster] 
eliminaCluster _ [] = []
eliminaCluster x (y:ys) 
    | x == y    = ys
    | otherwise = y : eliminaCluster x ys

-- Dada una lista de clusters, devuelve el par ( los 2 clusters mas cercanos, distancia entre ellos)
-- Para medir la distancia entre clusters utiliza la media
clustersDistanciaMinima :: [Cluster] -> ((Cluster, Cluster), Double)
clustersDistanciaMinima vss = head(sortBy sndTuple (calculaMatrixProximidad vss))

sndTuple (x1,y1) (x2,y2)
    | y1 > y2 = GT 
    | otherwise = LT


-- Obtiene una matriz simetrica que devuelve la distancia entre dos vectores cualesquiera optimo (simetrica)
calculaMatrixProximidad :: [[Vector ]] -> [(([Vector ], [Vector ]), Double)]
calculaMatrixProximidad [] = []
calculaMatrixProximidad (vs:vss) = calculaDistanciasAUnCluster vs vss ++ (calculaMatrixProximidad vss)

-- Distancia de todo los clusters a uno en concreto
calculaDistanciasAUnCluster vs [] = []
calculaDistanciasAUnCluster vs (xs:xss) = ((vs,xs), distanciaEntreClusters vs xs):(calculaDistanciasAUnCluster vs xss)

-- Distancia entre dos clusters cualesquiera
distanciaEntreClusters :: [Vector] -> [Vector] -> Double
distanciaEntreClusters v1 v2 = distEuclid vm1 vm2
    where vm1 = calculaMedia v1
          vm2 = calculaMedia v2

distEuclid :: Vector -> Vector -> Double
distEuclid v1 v2 
    | indices v1 == indices v2 = sqrt (sum[(x - y)**2 | (x,y) <- zip (elems v1) (elems v2)])
    | otherwise = error "Vectores incompatibles"


-- Calcular el punto medio de cada cluster
calculaMedia v = calculaMediaAux v 0 (replicate (fromIntegral (length (elems (v!!0)))) 0)


calculaMediaAux [] cont acc = listaVector [(acc!!(i-1)) / (fromIntegral(cont)) | i <- [1..(length acc)]]
calculaMediaAux (xm:xms) cont acc = calculaMediaAux xms (cont+1) [i + j  | (i,j) <- zip (elems xm) (acc)] 
