module ClusterAglomerativoArbol 
    (inicializaClusteringAglomerativoA,
    clusteringAglomerativoA,
    clusteringAglomerativoN
    ) where 

-------------------------------------------------------------------------------
-- Descripcion general del modulo
-- Este modulo tiene por objeto implementar el algoritmo de clustering aglomerativo
-- modelandolo mediante un dendograma.

-- El algoritmo comienza con un cluster por cada punto (vector). En las sucesivas 
-- iteraciones del algoritmo, se toman los dos clusters mas proximos y se fusionan.
-- El algoritmo finaliza cuando todos los vectores pertenecen al mismo cluster.

-- Imagen de referencia: https://estrategiastrading.com/wp-content/uploads/2019/07/ejemplo_dendrograma.png
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Modulos auxiliares importados
import Data.Array
import Data.List
import System.Random
import Data.Maybe
import Data.Tree
import Distancias
---------------------------------

-------------------------------------------------------------------------------
-- Definicion de tipos

-- type Cluster = [Vector]              -- Recogido en Distancias.hs        
type IdCluster = [Int]                  -- Tipo para identificar a un cluster en el arbol
-- IdCluster = lista de indices de los clusters de nivel inferior contenidos en ese cluster
-- Como inicialmente los clusters son los puntos, el identificador tambien indica los vectores 
-- que agrupa ese cluster.
-- Permite que se pueda visualizar de manera correcta la formacion del dendograma (visualizar
-- todos los vectores en cada nivel haria ilegible el arbol)

data Arbol = H IdCluster Cluster | N IdCluster Cluster Arbol Arbol  
    deriving Eq                         -- Tipo "arbol"
-- Este tipo permite reflejar como se van uniendo los clusters. Redefinimos la funcion show 
-- para que solo muestre el id de los subclusters (nodos) por legibilidad del arbol. 
instance Show Arbol where 
    show (H id cluster)= "(H " ++ (show id) ++ ")" 
    show (N id cluster denizq dender) = "(N "++ (show id) ++ " "++ show (denizq) ++ " "++ show (dender) ++ ")"

type Dendogram = [ Arbol ]              -- Tipo "bosque"
-- Inicialmente, todos los vectores forman un cluster por si mismos (al principio solo hay hojas sin emparejar).
-- Conforme se van fusionando los clusters se van formando arboles en paralelo (bosque).
-- Hasta el final no se obtiene un único arbol como tal.
--------------------------------

-------------------------------------------------------------------------------
-- Lista de funciones "herramienta" del modulo

-- Funcion listaClustersActuales :: Dendogram -> [(IdCluster, Cluster)]
--         listaClustersActuales2 :: Dendogram -> [Cluster]
-- Dado un dendrograma obtiene el ultimo estado de los clusters. Tomar el ultimo estado es, 
-- en definitiva, tomar las raices de todos los arboles del dendograma. 

-- Parametros:
-- dendograma :: Dendogram                  Bosque de dependencia entre clusters
-- Resultado:
-- [(IdCluster, Cluster)]  / [Cluster]      Lista de clusters formados 

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


-- Funcion arbolAsociadoACluster :: Dendogram -> Cluster -> Maybe Arbol
-- Funcion que dado un cluster = [Vector] devuelve el arbol del dendograma asociado a 
-- ese cluster (arbol que tiene como raiz ese cluster)

-- Parametros:
-- dendogram :: Dendogram               Bosque de dependencia entre clusters
-- cluster :: cluster                   Cluster 
-- Resultado:
-- arbol :: Maybe Arbol                 Arbol asociado a ese cluster

-- Funciones asociadas:
-- datosClusterFromArbol :: Arbol -> (IdCluster, Cluster)   
--                                      Funcion que dado un arbol devuelve su cluster asociado 
--                                      (Identificador de la raiz, cluster raiz) 

arbolAsociadoACluster :: Dendogram -> Cluster -> Maybe Arbol
arbolAsociadoACluster [] cluster = Nothing
arbolAsociadoACluster (arbol: xs) cluster = if (clusterArbol == cluster) 
                                                then Just arbol 
                                            else 
                                                arbolAsociadoACluster xs cluster
    where (idNodo, clusterArbol) = datosClusterFromArbol arbol


datosClusterFromArbol :: Arbol -> (IdCluster, Cluster)
datosClusterFromArbol (N idCluster cluster izq der) = (idCluster, cluster)
datosClusterFromArbol (H idCluster cluster) = (idCluster, cluster)


-------------------------------------------------------------------------------
-- Lista de funciones "del algoritmo" del modulo

-- Obtiene el primer nivel a partir de los datos:
-- todos los elementos forman un cluster por si mismos
inicializaClusteringAglomerativoA :: [Vector] -> Dendogram
inicializaClusteringAglomerativoA puntosIniciales = [ (H [indice] [punto] ) | (indice, punto) <- zip [0..] puntosIniciales ]

-- Funcion previa al algoritmo de clustering, elige la forma de representacion de los cluster en la consola
--clusteringAglomerativo :: Dendogram -> String -> Arbol
clusteringAglomerativoA fdistancia dendogram modo
    | modo == "AI" = toDataTreeId (clusteringAglomerativoAux fdistancia dendogram)
    | modo == "AC" = toDataTreeCl (clusteringAglomerativoAux fdistancia dendogram)
    | otherwise = error "Modo no valido"

clusteringAglomerativoN fdistancia dendogram = clusteringAglomerativoAux fdistancia dendogram

-- Funcion base del algoritmo de clustering: obtiene la evolucion de la lista de clusters
clusteringAglomerativoAux :: Distancia -> Dendogram -> Arbol
clusteringAglomerativoAux fdistancia dendogram 
    | condParada        = head $ dendogram
    | otherwise         = clusteringAglomerativoAux fdistancia (calculaSiguienteNivel fdistancia dendogram)
    where   condParada = length dendogram == 1 -- Ya solo tenemos un arbol, hemos terminado de agrupar

-- Dado un nivel, toma la correspondiente lista de clusters, fusiona los dos clusters mas cercanos y devuelve el siguiente nivel
calculaSiguienteNivel :: Distancia -> Dendogram -> Dendogram
calculaSiguienteNivel fdistancia dendogram = newDendogram
    where   clustersMasCercanos@(c1,c2) = fst $ clustersDistanciaMinima fdistancia dendogram
            (c1Id, c1list) = datosClusterFromArbol c1
            (c2Id, c2list) = datosClusterFromArbol c2
            newClusterId = c1Id ++ c2Id
            newClusterList = c1list ++ c2list
            fusionClustersMasCercanos = (N newClusterId newClusterList c1 c2)
            newDendogram = fusionClustersMasCercanos : (eliminaCluster c2 (eliminaCluster c1 dendogram))
        
eliminaCluster :: Arbol -> Dendogram -> Dendogram
eliminaCluster _ [] = []
eliminaCluster x (y:ys) 
    | x == y    = ys
    | otherwise = y : eliminaCluster x ys

-- Dada una lista de clusters, devuelve el par ( los 2 clusters mas cercanos, distancia entre ellos)
-- Para medir la distancia entre clusters utiliza la media
clustersDistanciaMinima :: Distancia -> Dendogram -> ((Arbol, Arbol), Double)
clustersDistanciaMinima fdistancia d = ((fromJust (arbolAsociadoACluster d c1list), fromJust(arbolAsociadoACluster d c2list)), distancia)
    where   vss = listaClustersActuales2 d 
            ((c1list, c2list), distancia ) = head(sortBy sndTuple (calculaMatrixProximidad fdistancia vss))


sndTuple (x1,y1) (x2,y2)
    | y1 > y2 = GT 
    | otherwise = LT




-- Obtiene una matriz simetrica que devuelve la distancia entre dos vectores cualesquiera optimo (simetrica)
calculaMatrixProximidad :: Distancia -> [[Vector ]] -> [(([Vector ], [Vector ]), Double)]
calculaMatrixProximidad fdistancia [] = []
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

-- Transforma nuestra estructura de datos a una del tipo Data.Tree para poder representarla mejor
toDataTreeId (H id cl) = Node (show (id)) []
toDataTreeId (N id cl n1 n2 ) = Node (show (id)) [toDataTreeId n1, toDataTreeId n2]

-- Transforma nuestra estructura de datos a una del tipo Data.Tree para poder representarla mejor
toDataTreeCl (H id cl) = Node (show (cl)) []
toDataTreeCl (N id cl n1 n2 ) = Node (show (cl)) [toDataTreeCl n1, toDataTreeCl n2]

-- a = inicializaClusteringAglomerativo lv
-- d = clusteringAglomerativo distEuclidea a "AI"
-- e = toDataTree d
-- f = putStrLn $ drawTree e

-------------------------------------------------------------------------------
-- Codigo "de juguete" para pruebas unitarias
-- v1 = listaVector [2.0,0.0]
-- v2 = listaVector [10.0,20.0]
-- v3 = listaVector [2.0,1.0]
-- v4 = listaVector [15.0,7.0]
-- lv = [v1, v2, v3, v4]
-- 
-- 
-------------------------------------------------------------------------------
