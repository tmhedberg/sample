{-# LANGUAGE FlexibleInstances, FunctionalDependencies, ScopedTypeVariables #-}

-- | Polymorphic random sampling of containers
module Data.Sample where

import Control.Applicative
import Control.Monad.IO.Class

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
class (Num i, Random i) => Sample s i a | s -> i a where

  -- | Sample a container, using the provided RandomGem instance as the entropy
  -- source
  sample :: RandomGen rg => rg -> s -> (a, rg)
  sample rg s = let bi = baseIndex (undefined :: s)
                    (i, rg') = randomR (bi, bi + size s - 1) rg
                in (s `index` i, rg')

  -- | Sample a container, using I/O to retrieve the system's global entropy
  -- source
  sampleIO :: MonadIO io => s -> io a
  sampleIO s = liftIO $ fmap fst $ sample <$> getStdGen <*> pure s

  -- | Retrieve the item at the specified index from the container
  index :: s -> i -> a

  -- | The index of the first element in the container
  --
  -- Default: @0@
  --
  -- The argument is unused and may be 'undefined'; it is required for
  -- unambiguous instance resolution.
  baseIndex :: s -> i
  baseIndex _ = 0

  -- | Return the number of elements in the given container
  size :: s -> i

instance Sample (Set a) Int a where index = flip elemAt
                                    size = S.size

instance Sample [a] Int a where index = (!!)
                                size = length

instance Sample (Map k v) Int k where index = flip elemAt . keysSet
                                      size = M.size

-- | Wrapper for 'Map' for which the 'Sample' instance samples values instead of
-- keys
newtype ValMap k v = ValMap (Map k v)

instance Sample (ValMap k v) Int v where index (ValMap m) = (elems m !!)
                                         size (ValMap m) = M.size m
