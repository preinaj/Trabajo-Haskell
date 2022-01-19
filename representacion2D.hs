module Representacion2D(
  dibuja
) where


import CodeWorld
import Data.Array

type Vector = Array Int Double


listaColores = [red, green, blue, pink, purple, yellow, orange, brown, gray]

-- Recibe una lista de clusters, lista de puntos asociadas a clusters y lista de colores. Pinta cada punto de un color asociado al cluster.
-- De maximo tantos clusters como la longitud de la lista de colores
--main :: IO ()
dibuja m xm = drawingOf (representaCentros asociados & (representasClusers xm asociados))
  where asociados = asociaColorACluster m listaColores

representaCentros [] = blank
representaCentros (x:xs) = dibujaCentro x & representaCentros xs

dibujaCentro (xs,c) = translated x y (colored black (solidRectangle 0.01 0.01))
  where x = xs !! 0
        y = xs !! 1 

representasClusers :: [(Representacion2D.Vector,Representacion2D.Vector)] -> [([Double], Color)] -> Picture
representasClusers [] _ = blank
representasClusers (x:xs) ls = dibujaPunto x ls & representasClusers xs ls

dibujaPunto (a,b) ls = translated x1 x2 (colored (buscaColor y ls)  (solidCircle 0.01))
  where x = elems a
        y = elems b
        [x1,x2] = x

asociaColorACluster [] _ = []
asociaColorACluster (x:xs) lc = [(elems x,head lc)] ++ asociaColorACluster xs (tail lc)

buscaColor x [] = error "Lista vacia"
buscaColor x xs@(y:ys) 
  | x == fst(y) = snd(y)
  | otherwise = buscaColor x ys

m = [array (1,2) [(1,0.43773985165814205),(2,0.31522081252276424)],array (1,2) [(1,0.13360000000000002),(2,0.3464)],array (1,2) [(1,9.823529411764707e-2),(2,9.176470588235294e-2)],array (1,2) [(1,115.0),(2,0.35)]]