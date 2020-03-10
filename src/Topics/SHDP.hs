module Topics.SHDP
( SHDPSpec
,
) where

data SHDPSpec = SHDPSpec 
    { k :: Integer
    , dimension :: Integer
    , alpha :: Float
    , gamma :: Float
    , sigma :: Float
    , tau :: Float
    , kappa :: Float
    , eta :: Float
    , decay :: Float
    , iter :: Integer
    } deriving (Eq, Show, Read)
