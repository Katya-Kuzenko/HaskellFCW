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

{-
6. Напишіть функцію, що для заданого списку пар чисел, 
що представляють точки площини, знаходить максимальну відстань між точками.
-}

-- відстань = корінь ( (x2 - x1)^2 + (y2 - y1)^2 )
-- 6 

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
maxDist :: [(Double, Double)] -> Double
maxDist a = maximum [dist a1 a2 | a1 <- a, a2 <- a]

task6 = do
  print "Task 6"
  print $ maxDist [(1, 2), (3, 4), (5, 6), (7, 8)]