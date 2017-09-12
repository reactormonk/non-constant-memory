module Lib
    ( allInDir
    , valueToTypes
    , TestType
    ) where

import Conduit
import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.Functor.Foldable
import qualified Data.ByteString as B
import GHC.Exts (toList, fromList)
import Data.Functor.Classes
import Data.HashMap.Lazy (elems)
import GHC.Generics
import Control.DeepSeq

data TestType a = TestType
  { inner :: [a]
  , someStuff :: Int
  } | Done deriving (Show, Eq, Functor, Generic, NFData)

instance Show1 TestType where
  liftShowsPrec sp sl d (TestType ls val) = showsBinaryWith (liftShowsPrec sp sl) showsPrec  "TestType" d ls val
  liftShowsPrec _ _ _ Done = showString "Done"

instance Monoid (TestType a) where
  mappend (TestType i1 d1) (TestType i2 d2) = TestType (mappend i1 i2) (d1 + d2)
  mappend e Done = e
  mappend Done e = e
  mempty = TestType [] 0

instance Monoid (Fix TestType) where
  mappend a b = Fix (unfix a `mappend` unfix b)
  mempty = Fix mempty

valueToTypes :: Value -> (Fix TestType)
valueToTypes (Object ob) = Fix $ TestType (fmap valueToTypes $ elems ob) 2
valueToTypes (Array array) = Fix $ TestType (toList $ fmap valueToTypes array) 3
valueToTypes (String _) = Fix $ TestType [(Fix Done)] 4
valueToTypes (Number _) = Fix $ TestType [(Fix Done)] 5
valueToTypes (Bool _) = Fix $ TestType [(Fix Done)] 6
valueToTypes Null =  Fix $ TestType [(Fix Done)] 7

allInDir :: MonadResource m => Conduit FilePath m (Fix TestType)
allInDir =
  mapMC (\filePath -> fmap (filePath,) $ liftIO $ B.readFile filePath)
  .| mapC (fmap (parseOnly json'))
  .| mapC (uncurry parse)
  .| mapC (either (const $ Fix $ TestType [(Fix Done)] 10) id)
    where
      parse filePath = fmap (\input -> valueToTypes input)

deriving instance Generic (Fix (TestType))
deriving instance NFData (Fix (TestType))
