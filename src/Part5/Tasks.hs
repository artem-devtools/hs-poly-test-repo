module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ i [] = i
myFoldl f i (x:xs) = myFoldl f (f i x) xs 

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ i [] = i
myFoldr f i (x:xs) = f x (myFoldr f i xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f lst = myFoldr (\e acc -> (f e):acc) [] lst

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f lst = myFoldl (\acc e -> acc ++ (f e)) [] lst

myConcat :: [[a]] -> [a]
myConcat lst2D = myFoldl (\acc lst -> acc ++ lst) [] lst2D

myReverse :: [a] -> [a]
myReverse lst = myFoldl (\acc e -> e:acc) [] lst

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p lst = myFoldr (\e acc -> if p e then e:acc else acc) [] lst

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p lst = myFoldr (\e (accTrue, accFalse) -> if p e then (e:accTrue, accFalse) else (accTrue, e:accFalse)) ([], []) lst

