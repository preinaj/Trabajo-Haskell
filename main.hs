import ClusterAglomerativoArbol
import KMeans
import Data.Array
import Data.Tree
import Data.IORef
import System.IO.Unsafe

type Vector = Array Int Double

-- Funcion auxiliar temportal para trabajar con vectores de forma más sencilla (borrar cuando vayamos a entregar)
listaVector :: [Double] -> Vector
listaVector xs = array (1,length xs) [(y,x) | (x,y) <- (zip xs [1..length xs])]


v1 = listaVector [2.0,0.0]
v2 = listaVector [10.0,20.0]
v3 = listaVector [2.0,1.0]
v4 = listaVector [15.0,7.0]

lv = [v1, v2, v3, v4] -- Datos de prueba, borrar cuando podamos leer datos


main = do 
    putStr "Indique la carpeta con los datos: "
    datos <- getLine
    -- Procesamiento de los datos
    -- Comprobar archivo valido
    seleccionAlgoritmo datos
    
seleccionAlgoritmo datos = do    
    putStr "Seleccion el algoritmo a usar: kMeans (KM), clusterAglomerativo (CA): "
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
    putStr "Indique el numero de centros para el algoritmo: "
    x <- getLine -- Añadir comprobacion numero valido
    let m = read x :: Int
    putStr  "Indique que datos desea extraer: unicamente los centros de los clusters (M), centros y datos asociados a cada uno (CM)"
    modo <- getLine
    if modo == "M"
        then do
            res <- (kMeans m lv)
            putStrLn (show res)
        else 
            if modo == "CM"
                then do
                    res <- (kMeansCompleto m lv)
                    putStrLn (show res)
                else do
                    putStrLn "Introduzca un modo valido"
                    algKMeans datos

clustAglomerativo datos = do
    putStr "Seleccion el tipo de estructura de datos: listaEvolucion (LE), Arbol (A): "
    xs <- getLine
    if xs == "LE"
        then
            clustAglomerativo datos
        else do
            if xs == "A"
                then 
                    clustAglomerativoArbol datos
                else do
                    putStrLn "Introduzca una opción válida"
                    clustAglomerativo datos

clustAglomerativoArbol datos = do
    putStr "Seleccione la forma de representacion por pantalla: arbol de id (AI), arbol de clusters (AC), normal (N)"
    modo <- getLine
    let d = inicializaClusteringAglomerativo lv --Aqui tendrían que venir los datos de verdad
    if modo == "AC" || modo == "AI"
        then
            putStrLn $ drawTree (clusteringAglomerativo d modo)
        else
            if modo == "N"
                then
                    putStrLn ( show (clusteringAglomerativoN d))
                else do
                    putStrLn "Introduzca una opción válida"
                    clustAglomerativoArbol datos
