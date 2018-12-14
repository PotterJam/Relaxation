--module Parallel where

import Data.Vector (Vector)
import qualified Data.Vector.Split as V
import qualified Data.Vector as V
import Control.Monad
import Control.Parallel.Strategies
import Data.Vector.Strategies

type Elem = (Int, Double, Bool)

precision :: (Fractional a) => a
precision = 10.0 ^^ (-1)

-- Index info is added to each element.
-- This can be replaced with comonads in the future.
-- https://bartoszmilewski.com/2017/01/02/comonads/
initVec :: Int -> Vector Elem
initVec n = V.zipWith3 (,,) (V.enumFromTo 0 (size-1)) vec (V.replicate size True)
    where 
        size         = n*n
        zeros        = V.replicate (n-2) 0.0
        midRow       = V.cons 1.0 $ zeros V.++ V.singleton 1.0
        middle       = V.concat $ replicate (n-2) midRow
        topandbottom = V.replicate n 1.0
        vec          = topandbottom V.++ middle V.++ topandbottom

constIndex :: Int -> Int -> Bool
constIndex i dim = topRow || bottomRow || leftCol || rightCol
    where
       topRow    = i <= (dim-1)
       bottomRow =  i >= (dim*dim) - dim
       leftCol   = (mod i dim) == 0
       rightCol  = (mod i dim) == (dim-1)

-- unsafe, assumes elem has neighbours
avgNeighbors :: Int -> Elem -> Vector Elem -> Elem 
avgNeighbors dim (i, v, s) vec = (i, newV, dComp v newV)
    where
        (_, up, _)    = vec V.! (i-dim)
        (_, down, _)  = vec V.! (i+dim)
        (_, left, _)  = vec V.! (i-1)
        (_, right, _) = vec V.! (i+1)
        newV  = (up + down + left + right)/4
        dComp = (\a b -> abs (a-b) < precision)

getNewElem :: Int -> Vector Elem -> Elem -> Elem
getNewElem dim vec e@(i, _, _) = if constIndex i dim then e 
                                 else avgNeighbors dim e vec

relaxVec :: Int -> Vector Elem -> Vector Elem
relaxVec dim vec = if settled updatedVec then updatedVec 
                   else relaxVec dim updatedVec
    where 
        updatedVec = (V.map (getNewElem dim vec) vec) `using` (parVector 100)

settled :: Vector Elem -> Bool
settled = all (\(_, _, s) -> s == True)

prettyPrint :: Int -> Vector Elem -> IO ()
prettyPrint dim vec = do
    let valVec   = V.map (\(_, v, _) -> v) vec
        splitVec = V.chunksOf dim valVec
    mapM (putStrLn.show) splitVec
    return ()

main = do
    let dim        = 3000
        vec        = initVec dim
        relaxedVec = relaxVec dim vec
    prettyPrint dim relaxedVec
    return ()
        

