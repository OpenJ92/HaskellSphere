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

-- Zeros :: [Int] ->  NDArray Int
-- Zeros [0,0,0] = []
-- Zeros [0,0,n] = Value x : Zeros [0,0,(-) n 1] xs
-- Zeros [0,n,k] = Array [ Zeros [0,(-) n 1,k] xs ]
-- Zeros [n,j,k] = Array [ Zeros [(-) n 1,j,k] xs ]
