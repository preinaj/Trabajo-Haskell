import Text.CSV
import System.Directory (doesFileExist)
import Data.Array

import ClusterAglomerativoListaEvolucion
import Representacion2D
import Distancias
import ClusterAglomerativoArbol
import KMeans
import Data.Array
import Data.Tree
import Data.IORef
import System.IO.Unsafe
import Debug.Trace
import Data.Typeable
import Data.Maybe

data Dataset4Clustering  = Dataset4Clustering {
    nombre :: String, 
    cabecera :: [String],
    datos :: [Vector]
    }
    deriving (Show, Eq)


leeDataset :: IO ()
leeDataset = do
    putChar '\n'
    putStrLn "--------------------------------------"
    putStrLn "       ELECCION DEL DATASET"
    putStrLn "--------------------------------------"
    putChar '\n'
    putStr "Introduce el nombre del fichero: "
    nombreFich <- getLine
    putChar '\n'
    exists <- doesFileExist nombreFich
    if exists then do
        putStrLn ("Leyendo fichero "++nombreFich++"...")
        putChar '\n'
        parseadorCSV nombreFich
    else do
        putStrLn "El fichero introducido no existe. Por favor, pruebe de nuevo"
        putChar '\n'
        leeDataset


parseadorCSV :: String -> IO()
parseadorCSV nombreFich = do
    input <- readFile nombreFich
    let csv = parseCSV nombreFich input
    let filas = case csv of
            (Right lineas) -> lineas
            _ -> []
    -- putStrLn (show filas)
    let filasValidas = filter (\x -> length x > 1) filas
    -- putStrLn (show filasValidas)
    if (length filasValidas < 2) then do
        putStrLn "\n Formato de fichero incorrecto"
        leeDataset
    else do 
        -- putStrLn $ printCSV filasValidas
        let cabecera = head filasValidas
        let contenido = tail filasValidas
        -- Tam dataSet
        let nc = length (head contenido)
        let nf = length contenido
        let dataset = Dataset4Clustering {
            nombre = nombreFich,
            cabecera = cabecera,
            datos = [ fila2Array fila | fila <- contenido ]
        }
        -- putStrLn (show [fila2Array fila | fila <- contenido] )
        -- aqui habria que llamar al main o a lo que use los datos para empezar el algoritmo
        -- putStrLn (show dataset)
        putStrLn (show (datos dataset))
        seleccionAlgoritmo (datos dataset)


fila2Array fila = array (1,l) filaDouble
    where   filaDouble = [ (ind, ((read dato) :: Double)) | (ind,dato) <- zip [1..] fila]
            l = length fila
            -- filaPrueba = trace ("DEGUB: "++ show filaDouble ) (head filaDouble)


seleccionAlgoritmo datos = do  
    putChar '\n'
    putStrLn "--------------------------------------"
    putStrLn "       ELECCION DE ALGORITMO"
    putStrLn "--------------------------------------"
    putChar '\n'  
    putStr "Seleccione el algoritmo a usar: kMeans (KM), clusterAglomerativo (CA): "
    xs <- getLine
    if xs == "KM"
        then
            algKMeans datos
        else do
            if xs == "CA"
                then
                    clustAglomerativo datos
                else do
                    putStrLn "Introduzca una opción válida"
                    seleccionAlgoritmo datos

algKMeans datos = do
    putChar '\n'
    putStrLn "--------------------------------------"
    putStrLn "       ALGORITMO DE K-MEDIAS"
    putStrLn "--------------------------------------"
    putChar '\n'
    distancia <- seleccionaDistancia
    putStrLn (show(typeOf distancia))
    putChar '\n'
    putStr "Indique el numero de centros para el algoritmo: "
    x <- getLine -- Añadir comprobacion numero valido
    let m = read x :: Int
    putChar '\n'
    putStr  "Indique que datos desea extraer: unicamente los centros de los clusters (M), centros y datos asociados a cada uno (CM): "
    modo <- getLine
    if modo == "M"
        then do
            res <- (kMeans m datos)
            let aux = (asocXM datos res)
            putStrLn (show res)
            representa res aux
        else 
            if modo == "CM"
                then do
                    res <- (kMeansCompleto m datos)
                    aux <- (kMeans m datos)
                    putStrLn (show res)
                    representa aux res
                else do
                    putChar '\n'
                    putStrLn "Introduzca un modo valido"
                    algKMeans datos
--representa :: [Vector] -> [(Vector, Vector)] -> IO ()
representa m xm = do
    if True -- Comprobar que tiene dos coordenadas
        then do
            putStr "¿Quiere una representacion grafica de los puntos: SI (S), NO (N)? "
            x <- getLine
            if x == "N"
                then
                    return ()
                else if x == "S" then
                    dibuja m xm
                else do
                    putStrLn "Introduzca una opcion valida"
                    representa m xm
    else 
        return ()

longTupla (a,b) = True

clustAglomerativo datos = do
    putChar '\n'
    putStrLn "--------------------------------------"
    putStrLn "ALGORITMO DE CLUSTERING AGLOMERATIVO"
    putStrLn "--------------------------------------"
    putChar '\n'
    distancia <- seleccionaDistancia
    putChar '\n'
    putStr "Seleccion el tipo de estructura de datos: listaEvolucion (LE), Arbol (A): "
    xs <- getLine
    if xs == "LE"
        then do
            let d = inicializaClusteringAglomerativoLE datos
            representaClusterAglomerativoLE (clusteringAglomerativoLE d)
        else do
            if xs == "A"
                then 
                    clustAglomerativoArbol datos
                else do
                    putChar '\n'
                    putStrLn "Introduzca una opción válida"
                    clustAglomerativo datos


representaClusterAglomerativoLE [] = return ()
representaClusterAglomerativoLE (x:xs) = do
    let nivel = fst' x
    let ls = snd' x
    putChar '\n'
    putChar '\n'
    putStrLn ("Nivel " ++ show nivel ++ " de la lista de evolucion: ")
    putStrLn "--------------------------------------" 
    representaUncluster ls 1
    representaClusterAglomerativoLE xs

representaUncluster [] _ = return ()
representaUncluster (x:xs) i = do
    putStrLn ("Cluster numero " ++ show i ++ ": " ++ show x)
    representaUncluster xs (i+1)

fst' (a,_,_) = a 
snd' (_,a,_) = a
thr' (_,_,a) = a

clustAglomerativoArbol datos = do
    putChar '\n'
    putStr "Seleccione la forma de representacion por pantalla: arbol de id (AI), arbol de clusters (AC), normal (N): "
    modo <- getLine
    let d = inicializaClusteringAglomerativoA datos --Aqui tendrían que venir los datos de verdad
    if modo == "AC" || modo == "AI"
        then
            putStrLn $ drawTree (clusteringAglomerativoA d modo)
        else
            if modo == "N"
                then
                    putStrLn ( show (clusteringAglomerativoN d))
                else do
                    putChar '\n'
                    putStrLn "Introduzca una opción válida"
                    clustAglomerativoArbol datos


seleccionaDistancia = do
    putStr "Indique el tipo de distancia a utilizar: Euclidea (DE), Manhattan (DM) o Hamming (DH): "
    distanciaStr <- getLine
    let distancia = case distanciaStr of
            "DE" -> Just distEuclidea
            "DM" -> Just distManhattan
            "DH" -> Just distHamming
            _    -> Nothing
    if (isNothing distancia) then do
        putChar '\n'
        putStrLn "Introduzca una opcion valida"
        seleccionaDistancia
    else do
        return (fromJust distancia)
