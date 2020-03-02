module Utils
( strongRightDistribute
, tupleFromList
) where

import Data.List (uncons)
import Data.Maybe (Maybe)

strongRightDistribute :: Monad f => (a, f b) -> f (a , b)
strongRightDistribute (x,y) = do
  y' <- y
  return (x, y')

tupleFromList :: [a] -> Maybe (a, a)
tupleFromList xs = do
  (x, ys) <- uncons xs
  (y, _ ) <- uncons ys
  return (x, y)
