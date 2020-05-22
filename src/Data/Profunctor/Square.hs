{-# LANGUAGE GADTs #-}
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
import Data.Functor.Compose.List
import Data.Profunctor.Composition.List
import qualified Data.Profunctor as P
import Data.Profunctor.Composition

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
fromHom = Square (Hom . P.dimap unId Id . unP)

-- |
-- > +-----+
-- > |     |
-- > |  @-(→)
-- > |     |
-- > +-----+
toHom :: Square '[] '[(->)] '[] '[]
toHom = Square (P . P.dimap unId Id . unHom)

-- * Squares for `Procompose`

-- |
-- >  +-----+
-- >  |   /-p
-- > q.p-@  |
-- >  |   \-q
-- >  +-----+
fromProcompose :: (P.Profunctor p, P.Profunctor q) => Square '[Procompose q p] '[p, q] '[] '[]
fromProcompose = Square ((\(Procompose q p) -> PComp (P.lmap unId p) (P (P.rmap Id q))) . unP)

-- |
-- >  +-----+
-- >  p-\   |
-- >  |  @-q.p
-- >  q-/   |
-- >  +-----+
toProcompose :: (P.Profunctor p, P.Profunctor q) => Square '[p, q] '[Procompose q p] '[] '[]
toProcompose = Square (P . (\(PComp p (P q)) -> Procompose (P.rmap Id q) (P.lmap unId p)))
