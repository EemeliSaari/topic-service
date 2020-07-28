-- chrisdone: https://gist.github.com/chrisdone/7dddadd089e6a5d2e3e9445c4692d2c2

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}


module Data.Defaults where

-- | Purpose of a data type.
data Purpose
  = Defaults -- For specifying defaults.
  | Complete -- For making a complete record.

-- | Required fields are not usable from a defaults spec.
type family Required (p :: Purpose) a where
  Required 'Defaults a  = () -- When we're defining defaults, required fields are ().
  Required 'Complete a = a