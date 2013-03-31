{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

module Sample where

import Control.Monad.IO.Class

import Data.Functor
import Data.Map hiding (elemAt, size)
import qualified Data.Map as M
import Data.Set hiding (elems, size)
import qualified Data.Set as S

import System.Random

class Sample s a | s -> a where
  sample :: RandomGen rg => rg -> s -> (a, rg)
  sample rg s = let (i, rg') = randomR (0, size s - 1) rg in (s `index` i, rg')
  sampleIO :: MonadIO io => s -> io a
  sampleIO s = liftIO $ (s`index`) <$> randomRIO (0, size s - 1)
  index :: s -> Int -> a
  size :: s -> Int

instance Sample (Set a) a where index = flip elemAt
                                size = S.size

instance Sample [a] a where index = (!!)
                            size = length

instance Sample (Map k v) k where index = flip elemAt . keysSet
                                  size = M.size

newtype ValMap k v = ValMap (Map k v)

instance Sample (ValMap k v) v where index (ValMap m) = (elems m !!)
                                     size (ValMap m) = M.size m
