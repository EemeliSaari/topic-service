module Topics.LDA
( LDASpec(..)
) where

data LDASpec = LDASpec 
    { topic_n :: Integer
    , alpha :: Float
    , eta :: Float
    , decay :: Float
    , iter :: Integer
    } deriving (Show, Read)

