{-
1. Напишіть функцію bin2int перетворення бінарного числа заданого рядком у число. Наприклад:
bin2int “000110” => 6 
-}

-- 1
bin2int :: String -> Int
bin2int = foldl (\acc x -> acc * 2 + read [x]) 0

task1 = do
  print "Task 1"
  print $ bin2int "000110"  -- 6
