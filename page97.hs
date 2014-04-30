import Data.Char


loop :: Int -> String -> Int
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
loop acc [] = acc

asInt :: String -> Int
asInt xs = loop 0 xs

asInt_fold :: String -> Int
asInt_fold xs = foldl parseInt 0 xs
   where parseInt acc x | 0 '-' = 0 :: Int
                        | acc x = (acc * 10) + (digitToInt x)


