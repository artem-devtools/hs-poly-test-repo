module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist list = l2r $ reverse list
    where l2r [] = REmpty
          l2r (x:xs) = (l2r xs) :< x 

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
  --show :: ReverseList a -> String
    show REmpty = "[]"
    show rlist = "[" ++ (join rlist) ++ "]"
        where join (REmpty :< x) = show x
              join (xs :< x) = (join xs) ++ "," ++ (show x)
    --showsPrec = notImplementedYet

instance (Eq a) => Eq (ReverseList a) where
  --(==) :: ReverseList a -> ReverseList a -> Bool
    (==) REmpty REmpty = True
    (==) (as :< a) (bs :< b) = (a == b) && (as == bs)
    (==) _ _ = False
    --(/=) = notImplementedYet

instance Semigroup (ReverseList a) where
  --(<>) :: ReverseList a -> ReverseList a -> ReverseList a
    (<>) as REmpty = as
    (<>) REmpty bs = bs
    (<>) as (bs :< b) = (as <> bs) :< b

instance Monoid (ReverseList a) where
  --mempty :: ReverseList a  
    mempty = REmpty

instance Functor ReverseList where
  --fmap :: (a -> b) -> ReverseList a -> ReverseList b
    fmap _ (REmpty) = REmpty
    fmap mf (xs :< x) = (fmap mf xs) :< (mf x)

instance Applicative ReverseList where
  --pure :: a -> ReverseList a
    pure x  = REmpty :< x
  --(<*>) :: ReverseList (a -> b) -> ReverseList a -> ReverseList b
    (<*>) REmpty _ = REmpty
    (<*>) _ REmpty = REmpty 
    (<*>) (fs :< f) as = (fs <*> as) <> (f <$> as)

instance Monad ReverseList where
  --(>>=) :: ReverseList a -> (a -> ReverseList b) -> ReverseList b
    (>>=) REmpty _ = REmpty
    (>>=) (as :< a) farb = (as >>= farb) <> (farb a)