import           Data.List
import qualified Data.Set            as Set
import qualified Data.Vector         as B (Vector)
import qualified Data.Vector.Generic as V
import           Data.Vector.Unboxed as U (Unbox, Vector)

type Matrix a = B.Vector (Vector a)

data IP = IP (Vector Int)
             (Vector Int)
             (Matrix Int)

data Graph a = Graph (Set.Set a) (Set.Set (a, a))
  deriving Show

myIP :: IP
myIP = IP obj rhs amat
  where
    obj = V.fromList [1, 2, 3]
    rhs = V.fromList [5, 5, 5]
    amat = V.fromList $ map V.fromList $ transpose
      [ [1, 0, 0]
      , [0, 1, 0]
      , [0, 0, 1]
      ]

createGraph :: IP -> Graph (Vector Int)
createGraph (IP c b a) =
  createGraph' c a distCheck origin (Graph Set.empty Set.empty)
  where
    origin :: (Unbox a, Num a) => Vector a
    origin      = V.replicate dim 0
    dim         = V.length c
    b'          = toDblVec b
    maxEntry    = max (V.maximum $ V.map V.maximum a) (V.maximum b)
    maxDist     = 2 * maxEntry * dim
    scaledB     = (1.0 / dot b' b') *^ b'
    distCheck p =
      let p' = toDblVec p
          nearest = dot p' b' *^ scaledB
      in floor (normInf (p' ^-^ nearest)) <= maxDist &&
         (between nearest b' origin || floor (normInf (p' ^-^ b')) <= maxDist
                                    || floor (normInf p')          <= maxDist)

between :: (Unbox a, Ord a)
        => Vector a -> Vector a -> Vector a -> Bool
between vx va vb = V.and $ V.zipWith3 (\x a b -> x >= a && x <= b) vx va vb

createGraph' :: Vector Int -> Matrix Int
             -> (Vector Int -> Bool)
             -> Vector Int
             -> Graph (Vector Int)
             -> Graph (Vector Int)
createGraph' c a distCheck point graph =
  V.foldl' insertAndContinue graph $ V.filter distCheck $ V.map (^+^point) a
  where
    insertAndContinue (Graph n e) p =
      let e' = Set.insert (point, p) e
      in if Set.member p n
           then Graph n e'
           else createGraph' c a distCheck p (Graph (Set.insert p n) e')

toDblVec :: (Unbox n, Integral n) => Vector n -> Vector Double
toDblVec = V.map fromIntegral

(^+^) :: (Unbox a, Num a) => Vector a -> Vector a -> Vector a
(^+^) = V.zipWith (+)

(^-^) :: (Unbox a, Num a) => Vector a -> Vector a -> Vector a
(^-^) = V.zipWith (-)

(*^) :: (Unbox a, Num a) => a -> Vector a -> Vector a
s *^ v = V.map (*s) v

dot :: (Unbox a, Num a) => Vector a -> Vector a -> a
dot v u = V.sum $ V.zipWith (*) v u

normInf :: (Ord a, Unbox a, Num a) => Vector a -> a
normInf v = V.maximum $ V.map abs v

main :: IO ()
main = print $ createGraph myIP
