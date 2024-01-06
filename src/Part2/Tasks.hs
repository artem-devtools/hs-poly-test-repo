module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm Plus a b
infixl 6 |+|
(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm Minus a b
infixl 6 |-|
(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm Times a b
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
-- I hate haskell for that:
replaceVar varName replacement expression = case expression of
   IntConstant _ -> expression
   Variable name -> if name == varName then replacement else expression
   BinaryTerm op lhv rhv -> BinaryTerm op (replace lhv) (replace rhv)
      where replace = replaceVar varName replacement

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm op (IntConstant x) (IntConstant y)) = IntConstant result 
   where 
      result = case op of
         Plus -> x + y
         Minus -> x - y
         Times -> x * y
evaluate (BinaryTerm op lhv rhv) = let binTerm = BinaryTerm op (evaluate lhv) (evaluate rhv) in
   case (lhv, rhv) of
      (BinaryTerm _ _ _, BinaryTerm _ _ _) -> evaluate binTerm
      _ -> binTerm
evaluate expression = expression