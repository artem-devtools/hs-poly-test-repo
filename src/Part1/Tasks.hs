module Part1.Tasks where

import Util(notImplementedYet)

-- Проиграл и сдался с позором
-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin = notImplementedYet

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos = notImplementedYet

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y
    | (d >= 1) && (m >= 1 && m <= 12) && (y >= 0) = isDateCorrect' d m y
    | otherwise = False

isDateCorrect':: Integer -> Integer -> Integer -> Bool
isDateCorrect' d 01 _ = d <= 31
isDateCorrect' d 02 y
  | (((y `mod` 4) == 0) && ((y `mod` 100) /= 0)) || ((y `mod` 400) == 0) = d <= 29
  | otherwise = d <= 28
isDateCorrect' d 03 _ = d <= 31
isDateCorrect' d 04 _ = d <= 30
isDateCorrect' d 05 _ = d <= 31
isDateCorrect' d 06 _ = d <= 30
isDateCorrect' d 07 _ = d <= 31
isDateCorrect' d 08 _ = d <= 31
isDateCorrect' d 09 _ = d <= 30
isDateCorrect' d 10 _ = d <= 31
isDateCorrect' d 11 _ = d <= 30
isDateCorrect' d 12 _ = d <= 31

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x p = aux x x p
  where 
    aux x' acc p'
      | p' == 1 = acc
      | otherwise = aux x' (acc * x') (p' - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | n <= 3 = True
  | ((n `mod` 2) == 0) || ((n `mod` 3) == 0) = False
  | otherwise = aux n 5
    where
      aux n' i
        | i >= (n' `div` 2) = True
        | ((n' `mod` i) == 0) || ((n' `mod` (i + 2)) == 0) = False
        | otherwise = aux n' (i + 6)


type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea [] = 0.0
shapeArea [_] = 0.0
shapeArea [_, _] = 0.0
shapeArea points = 1/2 * abs (sum1 + (xn * y1) - sum2 - (x1 * yn))
  where
    (x1, y1) = myListFirst points
    (xn, yn) = myListLast points
    sum1 = pairAdder (\(xi, _) -> \(_, yi1) -> xi * yi1) points
    sum2 = pairAdder (\(_, yi) -> \(xi1, _) -> xi1 * yi) points

myListFirst :: [a] -> a
myListFirst (x:xs) = x

myListLast :: [a] -> a
myListLast [x] = x
myListLast (x:xs) = myListLast xs

pairAdder :: (Point2D -> Point2D -> Double) -> [Point2D] -> Double
pairAdder _ [p] = 0.0
pairAdder f (pi:pi1:ps) = (f pi pi1) + (pairAdder f (pi1:ps))


-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
-- -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | (a > b + c) || (b > a + c) || (c > a + b) = -1
  | (a*a > b*b + c*c) || (b*b > a*a + c*c) || (c*c > a*a + b*b) = 0
  | (a*a < b*b + c*c) && (b*b < a*a + c*c) && (c*c < a*a + b*b) = 1
  | otherwise = 2
