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
-- >  +-f-g-+
-- >  | v v |
-- >  | \@/ |
-- >  |  v  |
-- >  +-g.f-+
toCompose :: (Functor f, Functor g) => Square '[] '[] '[f, g] '[Compose g f]
toCompose = mkSquare ((Compose .) . fmap . fmap)
