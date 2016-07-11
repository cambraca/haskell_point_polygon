import Data.Ratio
import Data.List.Split (splitOn)
import Parroquias

--type Point = (Double, Double)
type Point = (Rational, Rational)
type Polygon = [Point]
data Line = Sloped {lineSlope, lineYIntercept :: Rational} |
            Vert {lineXIntercept :: Rational}

polygonSides :: Polygon -> [(Point, Point)]
polygonSides poly@(p1 : ps) = zip poly $ ps ++ [p1]

intersects :: Point -> Line -> Bool
{- @intersects (px, py) l@ is true if the ray {(x, py) | x ≥ px}
intersects l. -}
intersects (px, _)  (Vert xint)  = px <= xint
intersects (px, py) (Sloped m b) | m < 0     = py <= m * px + b
                                 | otherwise = py >= m * px + b

onLine :: Point -> Line -> Bool
{- Is the point on the line? -}
onLine (px, _)  (Vert xint)  = px == xint
onLine (px, py) (Sloped m b) = py == m * px + b

carrier :: (Point, Point) -> Line
{- Finds the line containing the given line segment. -}
carrier ((ax, ay), (bx, by)) | ax == bx  = Vert ax
                             | otherwise = Sloped slope yint
  where slope = (ay - by) / (ax - bx)
        yint = ay - slope * ax

between :: Ord a => a -> a -> a -> Bool
between x a b | a > b     = b <= x && x <= a
              | otherwise = a <= x && x <= b

inPolygon :: Point -> Polygon -> Bool
inPolygon p@(px, py) = f 0 . polygonSides
  where f n []                             = odd n
        f n (side : sides) | far           = f n       sides
                           | onSegment     = True
                           | rayIntersects = f (n + 1) sides
                           | otherwise     = f n       sides
          where far = not $ between py ay by
                onSegment | ay == by  = between px ax bx
                          | otherwise = p `onLine` line
                rayIntersects =
                    intersects p line &&
                    (py /= ay || by < py) &&
                    (py /= by || ay < py)
                ((ax, ay), (bx, by)) = side
                line = carrier side

polygons = [
  (1,[(100,100),(200,100),(200,200),(100,200)]),
  (1,[(200,200),(200,300),(300,200)]),
  (2,[(300,300),(400,300),(400,400),(300,400)])
  ]

determinePolygon :: Point -> [(Integer, Polygon)] -> Integer
determinePolygon point polygons | length matches == 0 = 0
                                | otherwise = fst (head matches)
  where
    matches = filter (inPolygon2 point) polygons
    inPolygon2 point polygonDefinition = inPolygon point (snd polygonDefinition)
--    inPolygon2 point polygonDefinition = inPolygon (adjustPoint point) (adjustPolygon $ snd polygonDefinition)
--    adjustPoint (a,b) = ((a+5)*30,-b)
--    adjustPolygon (x:[]) = (adjustPoint x:[])
--    adjustPolygon (x:xs) = (adjustPoint x:adjustPolygon xs)

readRational :: String -> Rational
readRational string = toRational (read string :: Double)

readPoint :: String -> Point
readPoint line = (readRational $ parts !! 0, readRational $ parts !! 1)
  where parts = splitOn "," line

doInteraction :: String -> String
doInteraction line = show (determinePolygon (readPoint line) parroquias)

main = interact $ unlines . (map doInteraction) . lines

--main = do
--  print $ determinePolygon (150,150) polygons
--  print $ determinePolygon (210,210) polygons
--  print $ determinePolygon (340,380) polygons

--  print $ determinePolygon (-0.294667,-78.458524) parroquias
--  print $ determinePolygon (-0.294742,-78.458605) parroquias
--  print $ determinePolygon (-0.243445,-78.511955) parroquias
--  print $ determinePolygon (-0.2196702,-78.4958925) parroquias
--  print $ determinePolygon (-0.219634839,-78.49592212) parroquias
--  print $ determinePolygon (-0.219684,-78.495981) parroquias
--  print $ determinePolygon (-0.24146,-78.507039) parroquias
--  print $ determinePolygon (-0.961444,-80.73478) parroquias
--  print $ determinePolygon (-0.982363,-80.711902) parroquias
--  print $ determinePolygon (-0.982254,-80.712377) parroquias
--  print $ determinePolygon (-0.982108,-80.712141) parroquias
--  print $ determinePolygon (-0.944644,-80.734434) parroquias
--  print $ determinePolygon (-0.9549,-80.709414) parroquias
--  print $ determinePolygon (-0.955627,-80.708935) parroquias
--  print $ determinePolygon (-0.956549,-80.7098) parroquias
