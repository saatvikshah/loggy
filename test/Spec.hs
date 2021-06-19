module Main (main) where

import Test.Hspec (hspec)

import Test.LoggyCore

main :: IO ()
main = hspec $ do
    loggycore