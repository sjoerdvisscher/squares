{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Functor.Square where

import Data.Square
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Profunctor

-- * Squares for @Identity@

-- |
-- >  +--I--+
-- >  |  v  |
-- >  |  @  |
-- >  |     |
-- >  +-----+
fromIdentity :: Square '[] '[] '[Identity] '[]
fromIdentity = mkSquare (. runIdentity)

-- |
-- > +-----+
-- > |     |
-- > |  @  |
-- > |  v  |
-- > +--I--+
toIdentity :: Square '[] '[] '[] '[Identity]
toIdentity = mkSquare (Identity .)

-- * Squares for `Compose`

-- |
-- >  +-g.f-+
-- >  |  v  |
-- >  | /@\ |
-- >  | v v |
-- >  +-f-g-+
fromCompose :: (Functor f, Functor g) => Square '[] '[] '[Compose g f] '[f, g]
fromCompose = mkSquare ((. getCompose) . fmap . fmap)

-- |
-- > +-----+
-- > f  /->f
-- > .>-@  |
-- > g  \->g
-- > +-----+
--
-- > fromComposeStar = fromLeft === fromCompose === toRight2
fromComposeStar :: (Functor f, Functor g) => Square '[Star (Compose f g)] '[Star f, Star g] '[] '[]
fromComposeStar = fromLeft === fromCompose === toRight2

-- |
-- > +-----+
-- > f<-\  g
-- > |  @-<.
-- > g<-/  f
-- > +-----+
--
-- > fromComposeCostar = fromRight === fromCompose === toLeft2
fromComposeCostar :: (Functor f, Functor g) => Square '[Costar f, Costar g] '[Costar (Compose g f)] '[] '[]
fromComposeCostar = fromRight === fromCompose === toLeft2

-- |
-- >  +-f-g-+
-- >  | v v |
-- >  | \@/ |
-- >  |  v  |
-- >  +-g.f-+
toCompose :: (Functor f, Functor g) => Square '[] '[] '[f, g] '[Compose g f]
toCompose = mkSquare ((Compose .) . fmap . fmap)

-- |
-- > +-----+
-- > f>-\  f
-- > |  @->.
-- > g>-/  g
-- > +-----+
--
-- > toComposeStar = fromLeft2 === toCompose === toRight
toComposeStar :: (Functor f, Functor g) => Square '[Star f, Star g] '[Star (Compose f g)] '[] '[]
toComposeStar = fromLeft2 === toCompose === toRight

-- |
-- > +-----+
-- > g  /-<f
-- > .<-@  |
-- > f  \-<g
-- > +-----+
--
-- > toComposeCostar = fromRight2 === toCompose === toLeft
toComposeCostar :: (Functor f, Functor g) => Square '[Costar (Compose g f)] '[Costar f, Costar g] '[] '[]
toComposeCostar = fromRight2 === toCompose === toLeft
