module Crypto.Group where

class Group a where
    gcompose :: a -> a -> a
    gpow :: Integral i => a -> i -> Maybe a
    ginv :: a -> Maybe a
