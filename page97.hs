import Data.Char
import Debug.Trace


loop :: Int -> String -> Int
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
loop acc [] = acc

asInt :: String -> Int
asInt xs = loop 0 xs

-- The rest of this exercise has broken text and I cba
asInt_fold :: String -> Int
asInt_fold ('-':xs) = -(asInt_fold xs)
asInt_fold [] = 0
asInt_fold xs = foldl parseInt 0 xs
   where parseInt acc x = acc * 10 + digitToInt x

myconcat :: [[a]] -> [a]
myconcat xs = foldr step [] xs
  where  step ys x = ys ++ x

-- Ooh, we can use fns in guard clauses because purity
mytakewhilerec :: (a -> Bool) -> [a] -> [a]
mytakewhilerec fn xs = step xs []
  where step [] ys = ys
        step (y:rest) ys | fn y = step rest (ys ++ [y])
                         | otherwise = ys

-- Returning an empty list
mytakewhile :: (a -> Bool) -> [a] -> [a]
mytakewhile fn coll = foldr step [] coll
   where step x ys | fn x = x : ys
                   | otherwise = []




