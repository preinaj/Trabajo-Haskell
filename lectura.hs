import Text.CSV
import System.Directory (doesFileExist)
import Data.Array

type Vector = Array Int Double
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
    let filasValidas = filter (\x -> length x == 2) filas
    if (length filasValidas < 2) then
        putStrLn "\n Formato de fichero incorrecto"
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
        -- aqui habria que llamar al main o a lo que use los datos para empezar el algoritmo
        putStrLn "--------------------------------------"

fila2Array fila = array (1,l) filaDouble
    where   filaDouble = [ (ind, ((read dato) :: Double)) | (ind,dato) <- zip [1..] fila]
            l = length (head fila)