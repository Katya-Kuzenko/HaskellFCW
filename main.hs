-- 1
bin2int :: String -> Int
bin2int = foldl (\acc x -> acc * 2 + read [x]) 0

task1 = do
  print "Task 1"
  print $ bin2int "000110"  -- 6

{-
3. Напишіть функцію splitBy :: String -> String -> [String] 
розбиття заданого рядка на підрядки заданою множиною символів:
splitBy “,” “Hello, World!” => [“Hello”, “ World!”]
-}

-- 3

splitBy :: String -> String -> [String]
splitBy delimiter [] = []
splitBy delimiter str =
  let (start, rest) = span (`elem` delimiter) str
  in case rest of
    [] -> [start]
    _ -> start : splitBy delimiter (tail rest)

task2 = do -- воно виводить якусь фігню:) 
  print "Task 2"
  print $ splitBy "," "Hello, World!" -- [“Hello”, “ World!”]