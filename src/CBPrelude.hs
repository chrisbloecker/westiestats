{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CBPrelude
  where

import ClassyPrelude

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

cross :: [a] -> [b] -> [(a,b)]
cross as bs = [(a,b) | a <- as, b <- bs]

is :: Eq b => (a -> b) -> b -> (a -> Bool)
is f v a = f a == v

filterOn :: (a -> b) -> (b -> Bool) -> [a] -> [a]
filterOn ab p = filter (p . ab)
