module NDArray where

data NDArray a = Array [ NDArray a ] | Value a deriving (Show)

partition' :: Int -> [a] -> [[a]]
partition' _ [] = []
partition' n vs = (take n vs) : partition' n (drop n vs)

ndarray :: Shape -> [a] -> NDArray a
ndarray [_]    vs = Array $ map (Value) vs
ndarray (n:ns) vs = Array $ map (ndarray ns) $ partition' m vs
                    where
                      m = div (length vs) n

fill :: a -> Shape ->  NDArray a
fill n [x]    = Array [ Value n   | _ <- [1..x]]
fill n (x:xs) = Array [ fill n xs | _ <- [1..x]]

zeros' :: Shape ->  NDArray Int
zeros' = fill 0

ones' :: Shape ->  NDArray Int
ones' = fill 1

apply :: NDArray a -> (a -> b) -> NDArray b
apply (Value a) p = Value ( p a )
apply (Array a) p = Array ( [ apply i p | i <- a ] )

shape :: NDArray a -> Shape
shape (Value x) = []
shape (Array x) = length x : shape (head x)

getValue :: [Int] -> NDArray a -> a
getValue _       (Value x) = x
getValue (i:idx) (Array x) = getValue idx (x!!i)

-- https://docs.scipy.org/doc/numpy/reference/arrays.ndarray.html
