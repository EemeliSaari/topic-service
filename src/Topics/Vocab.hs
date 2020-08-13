module Topics.Vocab
( Vocab
, buildVocab
, getIdx
, numTerms
, prepareTokens
, bow
, unique
) where

import qualified Data.Map as Map
import qualified Data.Set as Set


data Vocab = Vocab 
    { _size :: Int
    , _table :: (Map.Map String Integer)
    } deriving (Show)

unique :: Ord a => [a] -> [a]
unique x = Set.toList (Set.fromList x)

count :: Ord a => a -> [a] -> Int
count _ [] = 0
count x list = sum $ map (\a -> 1) $ filter (== x) list

bow :: Ord a => [a] -> [(a, Int)]
bow x = [(a, (count a x))|a <- unique x]

fromJust :: Maybe a -> a
fromJust Nothing = error "KeyError"
fromJust (Just x) = x

buildVocab :: [[String]] -> Vocab
buildVocab d = Vocab 
    { _size = (length uniq)
    , _table = Map.fromList [(y, x) |(x, y) <- zip [0..] uniq]
    } where
        uniq = unique (concat d)

getIdx :: Vocab -> String -> Int
getIdx (Vocab _ v) w = fromIntegral (fromJust (Map.lookup w v))

numTerms :: Vocab -> Int
numTerms (Vocab n _) = n

prepareTokens :: Vocab -> [[String]] -> [[Int]]
prepareTokens v t = map (\x -> map (\y -> getIdx v y) x) t
