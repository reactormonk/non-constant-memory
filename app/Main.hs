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
  let mapAction = runConduit $
          sourceTBMChan filenames
          .| allInDir
          .| mapC force
          .| sinkTBMChan trees False
  _ <- forkIO $ do
    withPool 10 $ \pool -> parallel_ pool (replicate 10 mapAction)
    atomically $ closeTBMChan trees
  stuff <- runCConduit $ sourceTBMChan trees
    =$=& foldlC (\x y -> x + someStuff (unfix y)) 0
  Prelude.print stuff
