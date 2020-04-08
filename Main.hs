module Main where

import Sphere
import NDArray

import System.Random 

newRand = randomIO :: IO Double

randomList :: Int -> [Float]
randomList seed = randoms (mkStdGen seed) :: [Float]

main :: IO ()
main = putStrLn "Hello, Haskell!"
