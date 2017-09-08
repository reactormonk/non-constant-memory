module Main where

import Lib
import Conduit

main :: IO ()
main = do
  allJson <- runResourceT $ runConduit $ sourceDirectoryDeep False "out/" .| allInDir
  print allJson
