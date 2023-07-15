{-# LANGUAGE DataKinds, RankNTypes, GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Profunctor.Kan.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Profunctor.Kan.Square where

import Data.Square
import Data.Profunctor
import Data.Profunctor.Composition
import Data.Functor.Compose.List
import Data.Profunctor.Ran

-- |
-- > +-----+
-- > j-\   |
-- > |  @--p
-- > R-/   |
-- > +-----+
ranSquare :: (Profunctor j, Profunctor p) => Square '[j, Ran j p] '[p] '[] '[]
ranSquare = mkSquare $ \(Procompose r j) -> runRan r j

-- |
-- > +-----+     +-----+
-- > j-\   |     |     |
-- > |  @--p ==> q--@--R
-- > q-/   |     |     |
-- > +-----+     +-----+
--
-- Any square like the one on the left factors through 'ranSquare'.
-- 'ranFactor' gives the remaining square.
ranFactor
  :: (Profunctor j, Profunctor p, Profunctor q)
  => Square '[j, q] '[p] '[] '[] -> Square '[q] '[Ran j p] '[] '[]
ranFactor sq = mkSquare $ \q -> Ran $ \j -> runSquare sq (Procompose q j)

-- |
-- > +-----+
-- > R-\   |
-- > |  @--p
-- > j-/   |
-- > +-----+
riftSquare :: (Profunctor j, Profunctor p) => Square '[Rift j p, j] '[p] '[] '[]
riftSquare = mkSquare $ \(Procompose j r) -> runRift r j

-- |
-- > +-----+     +-----+
-- > q-\   |     |     |
-- > |  @--p ==> q--@--R
-- > j-/   |     |     |
-- > +-----+     +-----+
--
-- Any square like the one on the left factors through 'riftSquare'.
-- 'riftFactor' gives the remaining square.
riftFactor
  :: (Profunctor j, Profunctor p, Profunctor q)
  => Square '[q, j] '[p] '[] '[] -> Square '[q] '[Rift j p] '[] '[]
riftFactor sq = mkSquare $ \q -> Rift $ \j -> runSquare sq (Procompose j q)
