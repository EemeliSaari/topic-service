module Topics.SHdp
( SHdpSpec
,
) where

data SHdpSpec = SHdpSpec 
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
