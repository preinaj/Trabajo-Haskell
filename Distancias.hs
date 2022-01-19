module Distancias
(   distEuclid,     -- Funciones de tipo: Vector a -> Vector a -> a
    distManh,       
    distHamming,
    listaVector,
    Vector
) where

import Data.Array
type Vector = Array Int Double

listaVector :: [Double] -> Vector
listaVector xs = array (1,length xs) [(y,x) | (x,y) <- (zip xs [1..length xs])]

-- Distancia Euclidea 
distEuclid :: Vector -> Vector -> Double
distEuclid v1 v2 
    | indices v1 == indices v2 = sqrt (sum[(x - y)**2 | (x,y) <- zip (elems v1) (elems v2)])
    | otherwise = error "Vectores incompatibles"

-- Distancia Manhattan
distManh :: Vector -> Vector -> Double
distManh v1 v2 
    | indices v1 == indices v2 = sum[abs (x - y) | (x,y) <- zip (elems v1) (elems v2)]
    | otherwise = error "Vectores incompatibles"

-- Distancia Hamming
distHamming :: Vector -> Vector -> Double
distHamming v1 v2 
    | indices v1 == indices v2 = sum[if x == y then 0 else 1 | (x,y) <- zip (elems v1) (elems v2)]
    | otherwise = error "Vectores incompatibles"