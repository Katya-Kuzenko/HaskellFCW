-- 1
bin2int :: String -> Int
bin2int = foldl (\acc x -> acc * 2 + read [x]) 0

task1 = do
  print "Task 1"
  print $ bin2int "000110"  -- 6

-- 3
splitBy :: String -> String -> [String]
splitBy delimiter [] = []
splitBy delimiter str =
  case dropWhile (`elem` delimiter) str of
    "" -> []
    rest -> start : splitBy delimiter (drop (length start + length delimiter) str)
      where (start, _) = break (`elem` delimiter) str

task3 = do 
  print "Task 2"
  print $ splitBy "," "Hello, World!" -- [“Hello”, “ World!”]


-- 5
data Tree a = Node a (Tree a) (Tree a) | Empty

search :: Tree a -> [a]
search tree = search' [tree]
  where 
    search' :: [Tree a] -> [a]
    search' [] = []
    search' (Empty : rest) = search' rest
    search' (Node v l r : rest) = v : search' (rest ++ [l, r])

task5 = do
  print "Task 5"
  let tree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Node 6 Empty Empty) Empty)
  print $ search tree


-- 6 
dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
maxDist :: [(Double, Double)] -> Double
maxDist a = maximum [dist a1 a2 | a1 <- a, a2 <- a]

task6 = do
  print "Task 6"
  print $ maxDist [(1, 2), (3, 4), (5, 6), (7, 8)]


-- 4
data Point = Point Double Double
data Circle = Circle Point Double -- точка визначає центр, друге значення - це радіус кола
data Triangle = Triangle Point Point Point -- точки визначають вершини трикутника
data Square = Square Point Double -- точка визначає центр, друге значення - сторона квадрата

class Area a where
  area :: a -> Double

instance Area Circle where
  area (Circle _ r) = pi * r^2

instance Area Triangle where
  area (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) = 0.5 * abs ((x1-x3)*(y2-y3) - (x2-x3)*(y1-y3))

instance Area Square where
  area (Square _ a) = a^2

task4 = do
  print "Task 4"
  print $ area (Circle (Point 0 0) 5)
  print $ area (Square (Point 0 0) 5)
  print $ area (Triangle (Point 0 0) (Point 1 0) (Point 0 1))

{-
2. Для заданого гетероморфного списку, визначеного типом
data HList a = Atom a | List [HList a]
напишіть функцію, що розділяє його на пару підсписків: один містить лише атомарні елементи заданого списку, другий — решту елементів.
Наприклад:
	List [Atom 1, List [Atom 2, Atom 3], Atom 4] 
=> (List [Atom 1, Atom 4], List [List [Atom 2, Atom 3]])

-}
{-
-- 2
data HList a = Atom a | List [HList a]
splitHList :: HList a -> (HList a, HList a)
splitHList (List list) = splitHList' list
  where 
    splitHList' :: [HList a] -> (HList a, HList a)
    splitHList' [] = (List [], List [])
    splitHList' ...

task2 = do
  print "Task 2"
-}


main = do
  task1
  task3
  task4
  task5
  task6
