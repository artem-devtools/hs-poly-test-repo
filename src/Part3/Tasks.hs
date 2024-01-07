module Part3.Tasks where

import Util (notImplementedYet)
import Data.List (sort, group, map, filter)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = (f n):(finc f (n + 1))

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x:(ff f (f x))

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq [] = -1
mostFreq numbers = digit
  where 
    chars = myFlatMap (\n -> show n) numbers
    sortedChars = sort chars
    groupedChars = group sortedChars
    longestString = findLongestString groupedChars
    digit = (read longestString :: Int) `mod` 10

findLongestString :: [String] -> String
findLongestString strings = s
  where 
    (s, l) = foldr (\str (s, l) -> let len = length str in if len > l then (str, len) else (s, l)) ([], 0) strings

myFlatMap :: (a -> [b]) -> [a] -> [b]
myFlatMap _ [] = []
myFlatMap f (x:xs) = (f x) ++ (myFlatMap f xs) 

-- let it be here
numberToDigits :: Int -> [Int]
numberToDigits 0 = []
numberToDigits n = (numberToDigits (n `div` 10)) ++ [n `mod` 10]

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = if xs `contains` x then uniq xs else x:(uniq xs)

contains :: (Eq a) => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) e = if (x == e) then True else contains xs e

-- let it be here
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (xi:xi1:xs) = if xi > xi1 then False else isAsc (xi1:xs)

-- Функция grokBy принимает на вход функцию F и список Lst и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy _ [] = []
grokBy f xs = result
  where
    fs = map f xs
    f2x = myAssociate fs xs
    origins fi = foldr (\(f, x) acc -> if f == fi then x:acc else acc) [] f2x
    result = map (\fi -> (fi, origins fi)) $ uniq fs

myAssociate :: [a] -> [b] -> [(a, b)]
myAssociate _ [] = []
myAssociate [] _ = []
myAssociate (a:as) (b:bs) = ((a, b):(myAssociate as bs))
