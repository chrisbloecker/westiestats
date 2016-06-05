{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CBPrelude
  where
--------------------------------------------------------------------------------
import           ClassyPrelude
import           Data.Text               (append)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

(><) :: Text -> Text -> Text
(><) = append

spack :: Show a => a -> Text
spack = pack . show

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

is :: Eq b => (a -> b) -> b -> (a -> Bool)
is f v a = f a == v

filterOn :: (a -> b) -> (b -> Bool) -> [a] -> [a]
filterOn ab p = filter (p . ab)

renderLen :: Show a => Int -> a -> Text
renderLen n = pack . take n . show

--------------------------------------------------------------------------------

renderTime :: Integer -> Text
renderTime t = intercalate ":" [hh t, mm t, ss t]
  where
    hh :: Integer -> Text
    hh t = let h = spack . flip div (60*60) $ t
           in if length h == 1
                then "0" >< h
                else h

    mm :: Integer -> Text
    mm t = let m = spack . flip div 60 . flip mod (60*60) $ t
           in if length m == 1
                then "0" >< m
                else m

    ss :: Integer -> Text
    ss t = let s = spack . flip mod 60 $ t
           in if length s == 1
                then "0" >< s
                else s

--------------------------------------------------------------------------------

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

trd4 :: (a,b,c,d) -> c
trd4 (_,_,c,_) = c
