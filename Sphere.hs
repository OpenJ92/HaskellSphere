module Sphere where

import NDArray
import Data.List

-- construct Sphere
type T = Float
type S = Float
type C = Float
type Vector = [Float]
data Sphere = Sphere C S T Sphere | One 
  deriving (Show)

makeSphere :: Vector -> Int -> Sphere
makeSphere _      0 = One
makeSphere (t:ts) n = Sphere ( cos t ) ( sin t ) t ( makeSphere ts ( (-) n 1 ) )

initSphere :: Int -> Sphere
initSphere n = makeSphere ( take ( (-) n 1 ) ( repeat 0 ) ) ( (-) n 1 )

sizeSphere :: Sphere -> Int
sizeSphere One               = 1
sizeSphere (Sphere _ _ _ cs) = 1 + sizeSphere cs
