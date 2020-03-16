module Utils
( both
, dropLast
, fromEither
, ifTrue
, maybeToRight
, splitOnLast
, strongRightDistribute
, tupleFromList
) where

import Data.Bifunctor (Bifunctor, bimap)
import Data.List (elemIndices, splitAt, uncons)
import Data.Maybe (Maybe, listToMaybe)

strongRightDistribute :: Monad f => (a, f b) -> f (a , b)
strongRightDistribute (x,y) = do
  y' <- y
  return (x, y')

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight a Nothing = Left a
maybeToRight _ (Just b) = Right b

tupleFromList :: [a] -> Either String (a, a)
tupleFromList xs = do
  (x, ys) <- maybeToRight "Empty list; expected at least 2 elements" $ uncons xs
  (y, _ ) <- maybeToRight "Singleton list; expected at least 2 elements" $ uncons ys
  return (x, y)

ifTrue :: (a -> Bool) -> a -> Maybe a
ifTrue f = listToMaybe . filter f . pure

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

splitOnLast :: Eq a => a -> [a] -> ([a], [a])
splitOnLast x xs
  | (x `elem` xs) && (xs /= []) = (splitAt $ ((+) 1) $ last $ elemIndices x xs) xs
  | otherwise                   = ([], xs)

dropLast :: [a] -> Maybe [a]
dropLast [] = Nothing
dropLast xs = Just $ reverse $ drop 1 $ reverse xs
