module Category.Closed where

class ClosedCat f where
  closedCon :: (CatCon f a, CatCon f b) :- CatCon f (a -> b)
