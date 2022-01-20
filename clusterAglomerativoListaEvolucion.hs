module ClusterAglomerativoListaEvolucion(
    inicializaClusteringAglomerativoLE,
    clusteringAglomerativoLE
) where


import Data.Array
import Data.List
import System.Random
import Distancias

-- Codigo para probarlo
-- > d = inicializaClusteringAglomerativo lv
-- > clusteringAglomerativo d


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

-------------------------------

-- Obtiene el primer nivel a partir de los datos:
-- todos los elementos forman un cluster por si mismos
inicializaClusteringAglomerativoLE :: [Vector] -> EvolucionClusters
inicializaClusteringAglomerativoLE puntosIniciales = [ ( 0, [ [punto] | punto <- puntosIniciales], length puntosIniciales ) ]

-- Funcion base del algoritmo de clustering: obtiene la evolucion de la lista de clusters
clusteringAglomerativoLE :: Distancia -> EvolucionClusters -> EvolucionClusters
clusteringAglomerativoLE fdistancia listaEvolucion 
    | condParada        = listaEvolucion
    | otherwise         = clusteringAglomerativoLE fdistancia (listaEvolucion ++ [ calculaSiguienteNivel fdistancia nivelActual ] )
    where   nivelActual@(numNivel, listclusters, numClusters) = last listaEvolucion
            condParada = length listclusters == 1

-- Dado un nivel, toma la correspondiente lista de clusters, fusiona los dos clusters mas cercanos y devuelve el siguiente nivel
calculaSiguienteNivel :: Distancia -> Nivel -> Nivel
calculaSiguienteNivel fdistancia nivelActual = ( numNivel+1, listClustersNueva, length listClustersNueva )
    where   (numNivel, listclusters, numClusters) = nivelActual
            clustersMasCercanos@(c1,c2) = fst $ clustersDistanciaMinima fdistancia listclusters
            fusionClustersMasCercanos = c1 ++ c2
            listClustersNueva =  fusionClustersMasCercanos : (eliminaCluster c2 (eliminaCluster c1 listclusters))

eliminaCluster :: Cluster -> [Cluster] -> [Cluster] 
eliminaCluster _ [] = []
eliminaCluster x (y:ys) 
    | x == y    = ys
    | otherwise = y : eliminaCluster x ys

-- Dada una lista de clusters, devuelve el par ( los 2 clusters mas cercanos, distancia entre ellos)
-- Para medir la distancia entre clusters utiliza la media
clustersDistanciaMinima :: Distancia -> [Cluster] -> ((Cluster, Cluster), Double)
clustersDistanciaMinima fdistancia vss = head(sortBy sndTuple (calculaMatrixProximidad fdistancia vss))

sndTuple (x1,y1) (x2,y2)
    | y1 > y2 = GT 
    | otherwise = LT


-- Obtiene una matriz simetrica que devuelve la distancia entre dos vectores cualesquiera optimo (simetrica)
calculaMatrixProximidad :: Distancia -> [[Vector ]] -> [(([Vector ], [Vector ]), Double)]
calculaMatrixProximidad _ [] = []
calculaMatrixProximidad fdistancia (vs:vss) = calculaDistanciasAUnCluster fdistancia vs vss ++ (calculaMatrixProximidad fdistancia vss)

-- Distancia de todo los clusters a uno en concreto
calculaDistanciasAUnCluster fdistancia vs [] = []
calculaDistanciasAUnCluster fdistancia vs (xs:xss) = ((vs,xs), distanciaEntreClusters fdistancia vs xs):(calculaDistanciasAUnCluster fdistancia vs xss)

-- Distancia entre dos clusters cualesquiera
distanciaEntreClusters :: Distancia -> [Vector] -> [Vector] -> Double
distanciaEntreClusters fdistancia v1 v2 = fdistancia vm1 vm2
    where vm1 = calculaMedia v1
          vm2 = calculaMedia v2

-- Calcular el punto medio de cada cluster
calculaMedia v = calculaMediaAux v 0 (replicate (fromIntegral (length (elems (v!!0)))) 0)


calculaMediaAux [] cont acc = listaVector [(acc!!(i-1)) / (fromIntegral(cont)) | i <- [1..(length acc)]]
calculaMediaAux (xm:xms) cont acc = calculaMediaAux xms (cont+1) [i + j  | (i,j) <- zip (elems xm) (acc)] 
