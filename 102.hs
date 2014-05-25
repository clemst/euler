--euler problem 102: find how many triangles contains the center
import System.IO
import Data.List.Split (splitOn)

data Point    = Point    Int Int           deriving (Show)
data Vect     = Vect     Int Int           deriving (Show)
data Triangle = Triangle Point Point Point deriving (Show)

-- given two points, gives a vector
vect :: Point -> Point -> Vect
vect (Point x y) (Point x' y') = Vect (x'-x) (y'-y)

-- determinant of two vectors 
det :: Vect -> Vect -> Int
det (Vect x y) (Vect x' y') = x*y' - y*x'

-- barycentric coordinates
bar :: Point -> Triangle -> (Int,Int,Int)
bar p (Triangle a b c) = (det (vect p b) (vect p c),
                         det (vect p c) (vect p a),
                         det (vect p a) (vect p b))

-- a point is inside a triangle if its barycentric coordinates all have the same sign
isInside :: Point -> Triangle -> Bool
isInside p t | s (bar p t) == (1,1,1) = True
             | s (bar p t) == (-1,-1,-1) = True
             | otherwise = False
    where s (a,b,c) = (signum a, signum b, signum c)

-- parse text file and make triangles
parseTriangles :: String -> [Triangle]
parseTriangles s = map t (map (splitOn ",") (lines s))
    where t (ax:ay:bx:by:cx:cy:[]) = Triangle (pt ax ay) (pt bx by) (pt cx cy)
          pt x y = Point (read x) (read y)

main = do
    contents <- readFile "triangles.txt"
    print $ length $ filter id $ map (isInside (Point 0 0)) (parseTriangles contents)
