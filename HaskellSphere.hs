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

flatten' :: [[a]] -> [a]
flatten' []       = []
flatten' (xs:xss) = xs ++ flatten' xss

tensorProduct' :: Sphere -> Sphere -> NDArray Float
tensorProduct' p q = ndarray ns [length p', length q'] 
                       where
                         p' = eval' p
                         q' = eval' q
                         ns = flatten' $ tensorProduct p' q'

scalarProduct' :: Sphere -> Sphere -> Float
scalarProduct' p q = sum [p''*q'' | (p'', q'') <- zip p' q']
                       where
                         p' = eval' p
                         q' = eval' q

shape :: NDArray a -> [Int]
shape (Value x) = []
shape (Array x) = length x : shape (head x)

getValue :: [Int] -> NDArray a -> a
getValue _       (Value x) = x
getValue (i:idx) (Array x) = getValue idx (x!!i)

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

