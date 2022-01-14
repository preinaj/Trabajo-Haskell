module ClusterAglomerativoArbol 
    (inicializaClusteringAglomerativo,
    clusteringAglomerativo,
    clusteringAglomerativoN
    ) where 


import Data.Array
import Data.List
import System.Random
import Data.Maybe
import Data.Tree
---------------------------------

-- Codigo para probarlo
-- > d = inicializaClusteringAglomerativo lv
-- > clusteringAglomerativo d

type Vector = Array Int Double
type Cluster = [Vector]          
type IdCluster = [Int]

data Nodo = H IdCluster Cluster | N IdCluster Cluster Nodo Nodo  
    deriving Eq

-- (Identificador del cluster , puntos que pertenecen al cluster)
-- Identificador del cluster = indice de los clusters de nivel inferior contenidos en ese cluster
-- Como inicialmente los clusters son los puntos, el identificador tambien nos dice
-- que vectores agrupa ese cluster hasta ese nivel

type Dendogram = [ Nodo ]   -- conforme se construye van quedando bosques (al principio hay hojas sin emparejar)
                            -- Hasta el final no se obtiene un arbol asi que hay que generalizar el concepto de arbol

instance Show Nodo where 
    show (H id cluster)= "(H " ++ (show id) ++ ")" 
    show (N id cluster denizq dender) = "(N "++ (show id) ++ " "++ show (denizq) ++ " "++ show (dender) ++ ")"

type Nivel = [([Int], Cluster)]

v1 = listaVector [2.0,0.0]
v2 = listaVector [10.0,20.0]
v3 = listaVector [2.0,1.0]
v4 = listaVector [15.0,7.0]

lv = [v1, v2, v3, v4]

-- d:: Dendogram 
-- d =  [ ( N [1,2] [v1, v2] (H [1] [v1]) (H [2] [v2] ) ), 
--             (H [3] [v3] ),
--             (H [4] [v4] ) ] 

-- Funcion auxiliar temportal para trabajar con vectores de forma más sencilla (borrar cuando vayamos a entregar)
listaVector :: [Double] -> Vector
listaVector xs = array (1,length xs) [(y,x) | (x,y) <- (zip xs [1..length xs])]

--------------------------------

-- Funcion que dado un dendrograma obtiene el ultimo estado de los clusters
listaClustersActuales :: Dendogram -> [(IdCluster, Cluster)]
listaClustersActuales [] = []
listaClustersActuales (arbol: xs) = obtenCluster arbol : listaClustersActuales xs
    where  obtenCluster (N id cluster izq der) = (id, cluster)
           obtenCluster (H id cluster) = (id, cluster)

listaClustersActuales2 :: Dendogram -> [Cluster]
listaClustersActuales2 [] = []
listaClustersActuales2 (arbol: xs) = obtenCluster arbol : listaClustersActuales2 xs
    where  obtenCluster (N id cluster izq der) = cluster
           obtenCluster (H id cluster) = cluster

-- Funcion que dado un cluster = [puntos] devuelve el nodo del dendograma asociado a ese cluster
nodoAsociadoACluster :: Dendogram -> Cluster -> Maybe Nodo
nodoAsociadoACluster [] cluster = Nothing
nodoAsociadoACluster (nodo: xs) cluster = if (clusterNodo == cluster) 
                                            then Just nodo 
                                        else (nodoAsociadoACluster xs cluster)
    where (idNodo, clusterNodo) = datosClusterFromArbol nodo

-- Funcion que dado un arbol obtiene el estado del ultimo cluster
datosClusterFromArbol :: Nodo -> (IdCluster, Cluster)
datosClusterFromArbol (N idCluster cluster izq der) = (idCluster, cluster)
datosClusterFromArbol (H idCluster cluster) = (idCluster, cluster)


-------------------------------

-- Obtiene el primer nivel a partir de los datos:
-- todos los elementos forman un cluster por si mismos
inicializaClusteringAglomerativo :: [Vector] -> Dendogram
inicializaClusteringAglomerativo puntosIniciales = [ (H [indice] [punto] ) | (indice, punto) <- zip [0..] puntosIniciales ]

-- Funcion previa al algoritmo de clustering, elige la forma de representacion de los cluster en la consola
--clusteringAglomerativo :: Dendogram -> String -> Nodo
clusteringAglomerativo dendogram modo
    | modo == "AI" = toDataTreeId (clusteringAglomerativoAux dendogram)
    | modo == "AC" = toDataTreeCl (clusteringAglomerativoAux dendogram)
    | otherwise = error "Modo no valido"

clusteringAglomerativoN dendogram = clusteringAglomerativoAux dendogram

-- Funcion base del algoritmo de clustering: obtiene la evolucion de la lista de clusters
clusteringAglomerativoAux :: Dendogram -> Nodo
clusteringAglomerativoAux dendogram 
    | condParada        = head $ dendogram
    | otherwise         = clusteringAglomerativoAux (calculaSiguienteNivel dendogram)
    where   condParada = length dendogram == 1 -- Ya solo tenemos un arbol, hemos terminado de agrupar

-- Dado un nivel, toma la correspondiente lista de clusters, fusiona los dos clusters mas cercanos y devuelve el siguiente nivel
calculaSiguienteNivel :: Dendogram -> Dendogram
calculaSiguienteNivel dendogram = newDendogram
    where   clustersMasCercanos@(c1,c2) = fst $ clustersDistanciaMinima dendogram
            (c1Id, c1list) = datosClusterFromArbol c1
            (c2Id, c2list) = datosClusterFromArbol c2
            newClusterId = c1Id ++ c2Id
            newClusterList = c1list ++ c2list
            fusionClustersMasCercanos = (N newClusterId newClusterList c1 c2)
            newDendogram = fusionClustersMasCercanos : (eliminaCluster c2 (eliminaCluster c1 dendogram))
        
eliminaCluster :: Nodo -> Dendogram -> Dendogram
eliminaCluster _ [] = []
eliminaCluster x (y:ys) 
    | x == y    = ys
    | otherwise = y : eliminaCluster x ys

-- Dada una lista de clusters, devuelve el par ( los 2 clusters mas cercanos, distancia entre ellos)
-- Para medir la distancia entre clusters utiliza la media
clustersDistanciaMinima :: Dendogram -> ((Nodo, Nodo), Double)
clustersDistanciaMinima d = ((fromJust (nodoAsociadoACluster d c1list), fromJust(nodoAsociadoACluster d c2list)), distancia)
    where   vss = listaClustersActuales2 d 
            ((c1list, c2list), distancia ) = head(sortBy sndTuple (calculaMatrixProximidad vss))


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

-- Transforma nuestra estructura de datos a una del tipo Data.Tree para poder representarla mejor
toDataTreeId (H id cl) = Node (show (id)) []
toDataTreeId (N id cl n1 n2 ) = Node (show (id)) [toDataTreeId n1, toDataTreeId n2]

-- Transforma nuestra estructura de datos a una del tipo Data.Tree para poder representarla mejor
toDataTreeCl (H id cl) = Node (show (cl)) []
toDataTreeCl (N id cl n1 n2 ) = Node (show (cl)) [toDataTreeCl n1, toDataTreeCl n2]

-- a = inicializaClusteringAglomerativo lv
-- d = clusteringAglomerativo a
-- e = toDataTree d
-- f = putStrLn $ drawTree e