{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

-- | Polymorphic random sampling of containers
module Data.Sample where

import Control.Monad.IO.Class

import Data.Functor
import Data.Map hiding (elemAt, size)
import qualified Data.Map as M
import Data.Set hiding (elems, size)
import qualified Data.Set as S

import System.Random

-- | The class of container types which can be randomly sampled
--
-- The default instance for 'Map' samples the keys of the map; to sample values
-- instead, wrap the 'Map' in a 'ValMap'.
--
-- Minimal complete definition: 'index' and 'size'
class Sample s a | s -> a where

  -- | Sample a container, using the provided RandomGem instance as the entropy
  -- source
  sample :: RandomGen rg => rg -> s -> (a, rg)
  sample rg s = let (i, rg') = randomR (0, size s - 1) rg in (s `index` i, rg')

  -- | Sample a container, using I/O to retrieve the system's global entropy
  -- source
  sampleIO :: MonadIO io => s -> io a
  sampleIO s = liftIO $ (s`index`) <$> randomRIO (0, size s - 1)

  -- | Retrieve the item at the specified index from the container
  --
  -- Indexing is assumed to begin at 0.
  index :: s -> Int -> a

  -- | Return the number of elements in the container
  size :: s -> Int

instance Sample (Set a) a where index = flip elemAt
                                size = S.size

instance Sample [a] a where index = (!!)
                            size = length

instance Sample (Map k v) k where index = flip elemAt . keysSet
                                  size = M.size

-- | Wrapper for 'Map' for which the 'Sample' instance samples values instead of
-- keys
newtype ValMap k v = ValMap (Map k v)

instance Sample (ValMap k v) v where index (ValMap m) = (elems m !!)
                                     size (ValMap m) = M.size m
