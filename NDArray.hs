module NDArray where

data NDArray a = Array [ NDArray a ] | Value a deriving (Show)

-- construct NDArray
ndarray :: [a] -> [Int] ->  NDArray a
ndarray ns [x]    = Array $ ndaValueUtil ns ((-) x 1)
ndarray ns (x:xs) = Array $ ndaUtil  ns (length ns)  ((-) x 1) x xs

ndaValueUtil :: [a] -> Int -> [NDArray a]
ndaValueUtil []     _  = []
ndaValueUtil (n:ns) x' = Value n : ndaValueUtil ns ((-) x' 1)

ndaUtil :: [a] -> Int -> Int -> Int -> [Int] -> [NDArray a]
ndaUtil ns _   0  _ xs = [ ndarray ns xs ]
ndaUtil ns lns x' x xs = ndarray (take (m) ns) xs : ndaUtil (drop (m) ns) lns ((-) x' 1) x xs
                        where
                          m = div lns x

fill :: a -> [Int] ->  NDArray a
fill n [x]    = Array [ Value n   | _ <- [1..x]]
fill n (x:xs) = Array [ fill n xs | _ <- [1..x]]

zeros' :: [Int] ->  NDArray Int
zeros' = fill 0


-- measure NDArray
shape :: NDArray a -> [Int]
shape (Value x) = []
shape (Array x) = length x : shape (head x)

getValue :: [Int] -> NDArray a -> a
getValue _       (Value x) = x
getValue (i:idx) (Array x) = getValue idx (x!!i)


