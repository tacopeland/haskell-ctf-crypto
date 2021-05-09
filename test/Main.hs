module Main where

import qualified CryptoTests as CT
import System.Exit

main :: IO ()
main = do
    good <- and <$> sequence [CT.runTests]
    if good then exitSuccess else exitFailure
