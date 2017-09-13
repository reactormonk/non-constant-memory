module Main where

import Lib
import Conduit
import Data.Vector (Vector)
import Data.Functor.Foldable (Fix, unfix)
import Data.Conduit.TMChan
import Data.Conduit.Async
import Control.Concurrent.Async
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TBMChan
import Control.DeepSeq
import Data.Foldable
import Debug.Trace
import GHC.Conc (atomically)
import Control.Concurrent.ParallelIO.Local

main :: IO ()
main = do
  filenames <- newTBMChanIO 1000
  _ <- forkIO $ runConduitRes $ sourceDirectoryDeep False "out/" .| sinkTBMChan filenames True
  trees <- newTBMChanIO 1000
  let foldChunks size =
        conduitVector size
        .| mapC (\x -> x :: Vector (Fix TestType)) -- the type inference isn't omniscient
        -- .| mapC traceShowId
        .| mapC (\v -> force $ fold v)
  let mapAction = runResourceT $ runConduit $
          sourceTBMChan filenames
          .| allInDir
          .| mapC force
          .| sinkTBMChan trees False
  _ <- forkIO $ do
    withPool 10 $ \pool -> parallel_ pool (replicate 10 mapAction)
    atomically $ closeTBMChan trees
  allJson <- runResourceT $ runCConduit $ sourceTBMChan trees
    =$=& foldChunks 100
    =$=& foldChunks 10
    =$=& foldChunks 10
    =$=& foldChunks 10
    =$=& foldlC (\x y -> force $ mappend x y) mempty
  Prelude.print $ someStuff $ unfix $ force allJson
