{-# LANGUAGE DataKinds, RankNTypes, GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Kan.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Functor.Kan.Square where

import Data.Square
import Data.Profunctor
import Data.Profunctor.Composition
import Data.Functor.Compose.List

-- | The left Kan extension of a functor @f@ along a profunctor @j@.
--
-- The left Kan extension of a functor @f@ along a functor @g@ is @'Lan' ('Data.Profunctor.Costar' g) f@.
data Lan j f b = forall a. Lan (j a b) (f a)

-- |
-- > +--f--+
-- > |  v  |
-- > j--@  |
-- > |  v  |
-- > +--L--+
lanSquare :: Square '[j] '[] '[f] '[Lan j f]
lanSquare = mkSquare Lan

-- |
-- > +--f--+
-- > |  v  |     +--L--+
-- > j-\|  |     |  v  |
-- > |  @  | ==> h--@  |
-- > h-/|  |     |  v  |
-- > |  v  |     +--g--+
-- > +--g--+
--
-- Any square like the one on the left factors through 'lanSquare'.
-- 'lanFactor' gives the remaining square.
lanFactor :: (Profunctor h, IsFList gs) => Square '[j, h] '[] '[f] gs -> Square '[h] '[] '[Lan j f] gs
lanFactor sq = mkSquare $ \h (Lan j f) -> runSquare sq (Procompose h j) f

-- | The right Kan extension of a functor @f@ along a profunctor @j@.
--
-- The right Kan extension of a functor @f@ along a functor @g@ is @'Ran' ('Data.Profunctor.Star' g) f@.
newtype Ran j f a = Ran { runRan :: forall b. j a b -> f b }

-- |
-- > +--R--+
-- > |  v  |
-- > j--@  |
-- > |  v  |
-- > +--g--+
ranSquare :: Square '[j] '[] '[Ran j g] '[g]
ranSquare = mkSquare $ flip runRan

-- |
-- > +--f--+
-- > |  v  |     +--f--+
-- > h-\|  |     |  v  |
-- > |  @  | ==> h--@  |
-- > j-/|  |     |  v  |
-- > |  v  |     +--R--+
-- > +--g--+
--
-- Any square like the one on the left factors through 'ranSquare'.
-- 'ranFactor' gives the remaining square.
ranFactor :: (Profunctor j, IsFList fs) => Square '[h, j] '[] fs '[g] -> Square '[h] '[] fs '[Ran j g]
ranFactor sq = mkSquare $ \h f -> Ran $ \j -> runSquare sq (Procompose j h) f
