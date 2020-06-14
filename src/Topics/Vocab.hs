module Topics.Vocab
( Vocab
, buildVocab
, getIdx
, numTerms
, prepareTokens
) where

import qualified Data.Map as Map
import qualified Data.Set as Set


data Vocab = Vocab 
    { _size :: Int
    , _table :: (Map.Map String Integer)
    } deriving (Show)

fromJust :: Maybe a -> a
fromJust Nothing = error "KeyError"
fromJust (Just x) = x

buildVocab :: [[String]] -> Vocab
buildVocab d = Vocab{_size = (length uniq), _table = Map.fromList [(y, x) |(x, y) <- zip [0..] uniq]}
    where
        uniq = Set.toList (Set.fromList (concat d))

getIdx :: Vocab -> String -> Int
getIdx (Vocab _ v) w = fromIntegral (fromJust (Map.lookup w v))

numTerms :: Vocab -> Int
numTerms (Vocab n _) = n

prepareTokens :: Vocab -> [[String]] -> [[Int]]
prepareTokens v t = map (\x -> map (\y -> getIdx v y) x) t
