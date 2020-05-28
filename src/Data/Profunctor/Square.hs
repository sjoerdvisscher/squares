{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Profunctor.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Profunctor.Square where

import Data.Square
import qualified Data.Profunctor as P
import Data.Profunctor.Composition
import Data.Bifunctor.Biff

-- * Squares for profunctor subclasses

-- |
-- > +-a⊗_-+
-- > |  v  |
-- > p--@--p
-- > |  v  |
-- > +-a⊗_-+
second :: P.Strong p => Square '[p] '[p] '[(,) a] '[(,) a]
second = mkSquare P.second'

-- |
-- > +-a⊕_-+
-- > |  v  |
-- > p--@--p
-- > |  v  |
-- > +-a⊕_-+
right :: P.Choice p => Square '[p] '[p] '[Either a] '[Either a]
right = mkSquare P.right'

-- |
-- > +-a→_-+
-- > |  v  |
-- > p--@--p
-- > |  v  |
-- > +-a→_-+
closed :: P.Closed p => Square '[p] '[p] '[(->) a] '[(->) a]
closed = mkSquare P.closed

-- |
-- > +--f--+
-- > |  v  |
-- > p--@--p
-- > |  v  |
-- > +--f--+
map :: (P.Mapping p, Functor f) => Square '[p] '[p] '[f] '[f]
map = mkSquare P.map'

-- * Squares for @(->)@

-- |
-- >  +-----+
-- >  |     |
-- > (→)-@  |
-- >  |     |
-- >  +-----+
fromHom :: Square '[(->)] '[] '[] '[]
fromHom = mkSquare id

-- |
-- > +-----+
-- > |     |
-- > |  @-(→)
-- > |     |
-- > +-----+
toHom :: Square '[] '[(->)] '[] '[]
toHom = mkSquare id

-- * Squares for `Procompose`

-- |
-- >  +-----+
-- >  |   /-p
-- > q.p-@  |
-- >  |   \-q
-- >  +-----+
fromProcompose :: (P.Profunctor p, P.Profunctor q) => Square '[Procompose q p] '[p, q] '[] '[]
fromProcompose = mkSquare id

-- |
-- >  +-----+
-- >  p-\   |
-- >  |  @-q.p
-- >  q-/   |
-- >  +-----+
toProcompose :: (P.Profunctor p, P.Profunctor q) => Square '[p, q] '[Procompose q p] '[] '[]
toProcompose = mkSquare id

-- * Squares for `Biff`

-- |
-- > +--f--+                                                       +--f--+
-- > |  v  |                                                             |
-- > B--@--q   Biff q f g is the "universal filler for the niche":       q
-- > |  v  |                                                             |
-- > +--g--+                                                       +--g--+
fromBiff :: P.Profunctor q => Square '[Biff q f g] '[q] '[f] '[g]
fromBiff = mkSquare runBiff

-- |
-- > +-h-f-+
-- > | v v |      +--h--+
-- > | \ / |      |  v  |
-- > p--@--q  ->  p--@--B
-- > | / \ |      |  v  |
-- > | v v |      +--k--+
-- > +-k-g-+
--
-- This is the universal property of `Biff`.
toBiff :: (P.Profunctor q, Functor f, Functor g) => Square '[p] '[q] '[h, f] '[k, g] -> Square '[p] '[Biff q f g] '[h] '[k]
toBiff sq = mkSquare (Biff . runSquare sq)
