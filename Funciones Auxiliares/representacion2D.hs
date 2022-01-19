module Representacion2D(
  main
) where


import CodeWorld
import Data.Array

type Vector = Array Int Double


listaColores = [black, red, green, blue, pink, purple, yellow, orange, brown, gray]

-- Recibe una lista de clusters, lista de puntos asociadas a clusters y lista de colores. Pinta cada punto de un color asociado al cluster.
-- De maximo tantos clusters como la longitud de la lista de colores
--main :: IO ()
main m xm = drawingOf (representasClusers xm asociados) 
  where asociados = asociaColorACluster m listaColores

representasClusers :: [(Main.Vector,Main.Vector)] -> [([Double], Color)] -> Picture
representasClusers [] _ = blank
representasClusers (x:xs) ls = dibujaPunto x ls & representasClusers xs ls

dibujaPunto (a,b) ls = translated x1 x2 (colored (buscaColor y ls)  (solidCircle 0.1))
  where x = elems a
        y = elems b
        [x1,x2] = x

asociaColorACluster [] _ = []
asociaColorACluster (x:xs) lc = [(elems x,head lc)] ++ asociaColorACluster xs (tail lc)

buscaColor x [] = error "Lista vacia"
buscaColor x xs@(y:ys) 
  | x == fst(y) = snd(y)
  | otherwise = buscaColor x ys