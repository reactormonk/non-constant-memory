module Main where

import Lib
import Conduit
import Data.Vector (Vector)
import Data.Functor.Foldable (Fix)
import Data.Conduit.TMChan
import Data.Conduit.Async
import Control.Concurrent.Async
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TBMChan
import Control.DeepSeq
import Data.Foldable

main :: IO ()
main = do
  files <- runConduitRes $ sourceDirectoryDeep False "out/" .| sinkList
  trees <- newTBMChanIO 1000
  let foldChunks size =
        conduitVector size
        .| mapC (\x -> x :: Vector (Fix TestType)) -- the type inference isn't omniscient
        .| mapC (\v -> force $ fold v)
  _ <- forkIO $ mapConcurrently_
    (\path ->
      runResourceT $ runConduit $
      yield path
      .| allInDir
      .| mapC force
      .| sinkTBMChan trees False) files
  allJson <-
    runResourceT $ runCConduit $ sourceTBMChan trees
    =$=& foldChunks 100
    =$=& foldChunks 10
    =$=& foldChunks 10
    =$=& foldChunks 10
    =$=& foldlC (\x y -> force $ mappend x y) mempty
  Prelude.print allJson
