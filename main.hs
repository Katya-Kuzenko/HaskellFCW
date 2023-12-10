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
  let tree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Node 6 Empty Empty) Empty)
  print $ search tree