import Data.List
import Data.Maybe

--Q1
--(i)

averageCalc :: Int -> Int -> Int -> Float
averageCalc x y z = (fromIntegral x + fromIntegral y + fromIntegral z)/3.0

howManyBelowAverage :: Int -> Int -> Int -> Int
howManyBelowAverage x y z = length [a | a <- [x,y,z], fromIntegral a < avg]
    where
        avg = averageCalc x y z

--(ii)
howManyBelowAverage2 :: Int -> Int -> Int -> Int
howManyBelowAverage2 x y z = length (filter (< averageCalc x y z) [fromIntegral x, fromIntegral y, fromIntegral z])

q1Test :: Int -> Int -> Int -> Bool
q1Test x y z = howManyBelowAverage x y z == howManyBelowAverage2 x y z

doc1Test :: Bool
doc1Test = q1Test 2016290 1906470 2000000

--Q2
pizzaPricing :: Float -> Int -> Int -> Float
pizzaPricing x y z = truncate'((surface x * 0.002 + surface x * 0.001 * fromIntegral y + fromIntegral z * 0.5) * 1.5)
surface :: Float -> Float
surface d = pi * (d/2) ^ 2
truncate' :: Float -> Float
truncate' x  = fromIntegral (floor (x * 10^2)) / 10^2

--Q3
--(i)
data Direction = North | West | South | East
              deriving (Show,Eq)

data RelativeDirection = GoForward | GoBack | GoLeft | GoRight
    deriving Show

followDirection :: (Int, Int) -> Direction -> (Int,Int)
followDirection (x ,y) North =(x, y + 1)
followDirection (x ,y) East = (x + 1, y)
followDirection (x ,y) South = (x, y - 1)
followDirection (x ,y) West = (x - 1, y)

--(ii)

followDirections :: (Int,Int) -> [Direction] -> (Int,Int)
followDirections (x,y) d = if length d > 1
    then followDirections (followDirection (x, y) (head d)) (tail d)
    else followDirection (x, y) (head d)

--(iii)

convert :: Direction -> [Direction] -> [RelativeDirection] -> [RelativeDirection]
convert d ds rs | null ds = rs
                            | d == head ds = convert d  (tail ds) (rs ++ [GoForward])
                            | d == North && head ds == East = convert (head ds) (tail ds) (rs ++ [GoRight])
                            | d == North && head ds == South = convert (head ds) (tail ds) (rs ++ [GoBack])
                            | d == North && head ds == West = convert (head ds) (tail ds) (rs ++ [GoLeft])
                            | d == South && head ds == West = convert (head ds) (tail ds) (rs ++ [GoRight])
                            | d == South && head ds == East = convert (head ds) (tail ds) (rs ++ [GoLeft])
                            | d == South && head ds == North = convert (head ds) (tail ds) (rs ++ [GoBack])
                            | d == West && head ds == East = convert (head ds) (tail ds) (rs ++ [GoBack])
                            | d == West && head ds == South = convert (head ds) (tail ds) (rs ++ [GoLeft])
                            | d == West && head ds == North = convert (head ds) (tail ds) (rs ++ [GoRight])
                            | d == East && head ds == West = convert (head ds) (tail ds) (rs ++ [GoBack])
                            | d == East && head ds == North = convert (head ds) (tail ds) (rs ++ [GoLeft])
                            | d == East && head ds == South = convert (head ds) (tail ds) (rs ++ [GoRight])

relativizeDirections :: Direction -> [Direction] -> [RelativeDirection]
relativizeDirections d ds = convert d ds []

--(iv)

sanitizeDirections :: [Direction] -> [Direction]
sanitizeDirections d = checkDirections d [(0,0)] []

checkDirections :: [Direction] -> [(Int, Int)] -> [Direction] -> [Direction]
checkDirections initialD p cleanedD
  | null initialD = cleanedD
  | isNothing (whenVisited (head initialD) p) = checkDirections (tail initialD) (p ++ [followDirection (last p) (head initialD)]) (cleanedD ++ [head initialD])
  | otherwise = checkDirections (tail initialD) (map (p !!) [0..(fromJust (whenVisited (head initialD) p)-1)] ++ [followDirection (last p) (head initialD)]) (map (cleanedD !!) [0..(fromJust (whenVisited (head initialD) p)-1)])

whenVisited :: Direction -> [(Int, Int)] -> Maybe Int
whenVisited d ps = elemIndex (followDirection (last ps) d) ps

--Q4
-- GetDirectionsOut output ends with a north due to the example code end point being (4,4) and not (4,3).
-- This therefore means that if you change the exampleMaze code end condition to (4,3)
-- like in the diagram in the assignment questions it will return the correct output.

data Orientation = H | V
                deriving (Show,Eq)
type Wall = (Int, Int, Orientation)
type Maze = ((Int,Int),[Wall])

data Pos
    = Pos Int Int           -- A position is composed of two Int coordinates
    deriving (Eq, Show)

exampleMaze :: Maze
exampleMaze = ((4,4), hWalls ++ vWalls)
    where vWalls = map (\ (i,j) -> (i,j,V))
                     [
                      (0,0),(0,1),(0,2),(0,3),
                            (1,1),(1,2),
                            (2,1),(2,2),
                                  (3,2), (3,3),
                      (4,0),(4,1),(4,2)
                     ]
hWalls = map (\ (i,j) -> (i,j,H))
           [
            (0,0),(1,0),(2,0),(3,0),
            (0,1),      (2,1),
                        (2,2),
            (0,4),(1,4),(2,4),(3,4)
           ]

-- get the direction the user is facing and tries to go left from their and rotates clockwise until it can move forwards 
-- it checks by using a function that takes a set of coordinates and says weather theres a wall in the way if their is it then rotates
-- it recursivly calls itself with the new coordinates and direction facing untill x/y is outside the maze border or all squares 
-- have been visited

getDirectionsOut :: Maze -> Maybe [Direction]
getDirectionsOut m = Just (findDirection m (Pos 0 0) North [])

checkLeft :: Maze -> Pos -> Direction -> Bool
checkLeft m (Pos x y) dir
  | dir == North = any (\w -> ( x, y, V) == w) (snd m)
  | dir == South = any (\w -> (x + 1, y, V) == w) (snd m)
  | dir == East = any (\w -> (x, y + 1, H) == w) (snd m)
  | otherwise = any (\w -> (x, y, H) == w) (snd m)

findDirection :: Maze -> Pos -> Direction -> [Direction] -> [Direction]
findDirection m p dir ds
  | p == uncurry Pos (fst m) = ds
  | checkLeft m p dir = findDirection m p (rotate (rotate (rotate dir))) ds
  | otherwise = findDirection m (translate p (rotate dir)) (rotate dir) (ds ++ [rotate dir])

rotate :: Direction -> Direction
rotate North = West
rotate East = North
rotate South = East
rotate West = South

translate :: Pos -> Direction -> Pos
translate (Pos x y) West = Pos (x - 1) y
translate (Pos x y) East = Pos (x + 1) y
translate (Pos x y) North = Pos x (y + 1)
translate (Pos x y) South = Pos x (y - 1)

--Q5

data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)
  deriving Show

ex1 = Unary
       (Unary
        (Unary
         (Unary
          (Unary
           (Unary
            (Unary (Leaf 0) 1)
            2)
           3)
          4)
         5)
        6)
       7
ex2 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Unary (Leaf 4) 5)
ex3 = Binary (Unary (Leaf 1) 2) 3 (Unary (Leaf 4) 5)
ex4 = Binary (Unary (Leaf 1) 2) 3 (Binary (Leaf 4) 5 (Leaf 10))
ex5 :: Btree (Integer, String)
ex5 = Binary (Binary (Leaf (0,"a"))
                     (1,"z")
                     (Leaf (2,"x")))
             (3,"y")
             (Binary (Leaf (4,"b"))
                     (5,"c")
                     (Leaf (6,"d")))
--test tree for Q5 part iii as had monomorphism restriction int and integer running this almost identicle tree alows that part to work (does not work for other parts)
ex5' :: Btree (Int, String)
ex5' = Binary (Binary (Leaf (0,"a"))
                     (1,"z")
                     (Leaf (2,"x")))
             (3,"y")
             (Binary (Leaf (4,"b"))
                     (5,"c")
                     (Leaf (6,"d")))

--(i)
complete :: Btree a -> Bool
complete tree = isComplete tree 0 (countNodes tree 0)


isComplete :: Btree a -> Int -> Int -> Bool
isComplete (Leaf _) index totNodes = index<=totNodes
isComplete (Binary l _ r) index totNodes = index < totNodes && isComplete l ((2*index)+1) totNodes && isComplete r ((2*index)+2) totNodes
isComplete (Unary l _) index totNodes = (index<totNodes) && isComplete l ((2*index)+1) totNodes

perfect :: Btree a -> Bool
perfect (Unary _ _) = False
perfect (Leaf _) = True
perfect (Binary l _ r) = (countNodes r 0 == countNodes l 0) && perfect l && perfect r

countNodes :: Btree a -> Int -> Int
countNodes (Leaf _) n = n
countNodes (Unary l _) n = countNodes l n+1
countNodes (Binary l _ r ) n = countNodes l n+1 + (countNodes r n+1)

--(ii)
lookupInSearchTree :: Integer -> Btree (Integer, a) -> Maybe a
lookupInSearchTree k (Leaf (a, b)) = if a==k
  then Just b
  else Nothing
lookupInSearchTree k (Unary l (a, b)) = if a==k
  then Just b
  else lookupInSearchTree k l
lookupInSearchTree k (Binary l (a, b) r) = if a==k
  then Just b
  else if k<a
    then lookupInSearchTree k l
    else lookupInSearchTree k r

--(iii)
insertInSearchTree :: Int -> a -> Btree (Int,a) -> Btree (Int,a)
insertInSearchTree n b (Leaf (x,v))       | n > x =  Unary (Leaf (x,v)) (n,b)
                                          | otherwise = Unary (Leaf (n,b)) (x,v)
insertInSearchTree n b (Binary l (x,v) r) | n > x = Binary l (x,v) (insertInSearchTree n b r)
                                          | otherwise = Binary (insertInSearchTree n b l) (x,v) r
insertInSearchTree n b (Unary l (x,v))    | n > x = Unary (insertInSearchTree x v l) (n,b)
                                          | otherwise = Unary (insertInSearchTree n b l) (x,v) 