import Prelude

type C = Float
type S = Float
type T = Float
type Theta = [Float]
type Vector = [Float]

data Sphere = Sphere C S T Sphere | One deriving (Show)

makeSphere :: Theta -> Int -> Sphere
makeSphere _ 0 = One
makeSphere (t:ts) n = Sphere ( cos t ) ( sin t ) t ( makeSphere ts ( (-) n 1 ) )

sizeSphere :: Sphere -> Int
sizeSphere One = 1
sizeSphere (Sphere _ _ _ cs) = 1 + sizeSphere cs

initSphere :: Int -> Sphere
initSphere n = makeSphere ( take n ( repeat 0 ) ) ( (-) n 1 )

updateSphere :: Sphere -> Theta -> Sphere
updateSphere s ts = makeSphere ts $ sizeSphere s

eval' :: Sphere -> Vector 
eval' One = [1.0]
eval' (Sphere c s _ cs) = ( map (*c) ( eval' cs ) ) ++ [ s ] 
