module NDArray where

import Data.List

type Shape = [Int]
data NDArray a = Array [ NDArray a ] | Value a deriving (Show)

instance Functor NDArray where
  fmap f (Value s ) = Value (f s)
  fmap f (Array fs) = Array (fmap (fmap f) fs)

partition' :: Int -> [a] -> [[a]]
partition' _ [] = []
partition' n vs = (take n vs) : partition' n (drop n vs)

ndarray :: Shape -> [a] -> NDArray a
ndarray [_]    vs = Array $ map (Value) vs
ndarray (n:ns) vs = Array $ map (ndarray ns) $ partition' m vs
                    where
                      m = div (length vs) n

fill :: a -> Shape ->  NDArray a
fill n shape = ndarray shape $ replicate (foldl' (*) 1 shape) n

zeros' :: Shape ->  NDArray Float
zeros' = fill 0

ones' :: Shape ->  NDArray Float
ones' = fill 1

shape :: NDArray a -> Shape
shape (Value x) = []
shape (Array x) = length x : shape (head x)

getValue :: [Int] -> NDArray a -> a
getValue _       (Value x) = x
getValue (i:idx) (Array x) = getValue idx (x!!i)

-- https://docs.scipy.org/doc/numpy/reference/arrays.ndarray.html

-- experiment with transpose with 2D array. How can one extend this to NDArray?
transpose':: [[a]] -> [[a]]
transpose' []         = repeat []
transpose' (row:rows) = zipWith (:) row (transpose' rows)
