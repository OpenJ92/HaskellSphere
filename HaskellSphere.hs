import Prelude

type C = Float
type S = Float
type T = Float
type Vector = [Float]
type Matrix = [[Float]]

data Sphere = Sphere C S T Sphere | One deriving (Show)
data NDArray a = Array [ NDArray a ] | Value a deriving (Show)

makeSphere :: Vector -> Int -> Sphere
makeSphere _      0 = One
makeSphere (t:ts) n = Sphere ( cos t ) ( sin t ) t ( makeSphere ts ( (-) n 1 ) )

initSphere :: Int -> Sphere
initSphere n = makeSphere ( take ( (-) n 1 ) ( repeat 0 ) ) ( (-) n 1 )

sizeSphere :: Sphere -> Int
sizeSphere One               = 1
sizeSphere (Sphere _ _ _ cs) = 1 + sizeSphere cs

updateSphere :: Sphere -> Vector -> Sphere
updateSphere s ts = makeSphere ts $ sizeSphere s

eval' :: Sphere -> Vector 
eval' One               = [1.0]
eval' (Sphere c s _ cs) = ( map (*c) ( eval' cs ) ) ++ [ s ] 

tensorProduct :: Vector -> Vector -> Matrix
tensorProduct []     _  = []
tensorProduct (x:xs) ys = map (*x) ys : tensorProduct xs ys

shape :: NDArray a -> [Int]
shape (Value x)      = []
shape (Array x) = length x : shape (head x)

getValue :: [Int] -> NDArray a -> a
getValue _       (Value x) = x
getValue (i:idx) (Array x) = getValue idx (x!!i)

-- This can be reconstructed as a mutually recursive set of functions. 
-- It might be nessesary to construct a recursive list construction so 
-- as to partition the input as we traverse the tree.

fill' :: [a] -> [Int] ->  NDArray a
fill' ns [x]    = Array $ loop' ns ((-) x 1)
fill' ns (x:xs) = Array $ loop  ns (length ns)  ((-) x 1) x xs

loop' :: [a] -> Int -> [NDArray a]
loop' []     _  = []
loop' (n:ns) x' = Value n : loop' ns ((-) x' 1)

loop :: [a] -> Int -> Int -> Int -> [Int] -> [NDArray a]
loop ns _   0  _ xs = [ fill' ns xs ]
loop ns lns x' x xs = fill' (take (div lns x) ns) xs : loop (drop (div lns x) ns) lns ((-) x' 1) x xs

fill :: a -> [Int] ->  NDArray a
fill n [x]    = Array [ Value n   | _ <- [1..x]]
fill n (x:xs) = Array [ fill n xs | _ <- [1..x] ]

zeros' :: [Int] ->  NDArray Int
zeros' = fill 0

ones' :: [Int] -> NDArray Int
ones' = fill 1
