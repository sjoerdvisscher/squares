{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Traversable.Square where

import Prelude hiding (traverse)
import Data.Square
import Data.Profunctor
import qualified Data.Traversable as T

-- |
-- > +--t--+
-- > |  v  |
-- > f>-T->f
-- > |  v  |
-- > +--t--+
--
-- `traverse` as a square.
--
-- Naturality law:
--
-- > +-----t--+     +--t-----+
-- > |     v  |     |  v     |
-- > f>-@->T->g === f>-T->@->g
-- > |     v  |     |  v     |
-- > +-----t--+     +--t-----+
--
-- Identity law:
--
-- > +--t--+     +--t--+
-- > |  v  |     |  |  |
-- > |  T  | === |  v  |
-- > |  v  |     |  |  |
-- > +--t--+     +--t--+
--
-- Composition law:
--
-- > +--t--+     +--t--+
-- > |  v  |     |  v  |
-- > f>-T->f     f>\|/>f
-- > |  v  | === |  T  |
-- > g>-T->g     g>/|\>g
-- > |  v  |     |  v  |
-- > +--t--+     +--t--+
traverse :: (Traversable t, Applicative f) => Square '[Star f] '[Star f] '[t] '[t]
traverse = mkSquare (Star . T.traverse . runStar)

-- |
-- > +-f-t---+
-- > | v v   |
-- > | \-@-\ |
-- > |   v v |
-- > +---t-f-+
--
-- @sequence = toRight ||| traverse ||| fromLeft@
sequence :: (Traversable t, Applicative f) => Square '[] '[] '[f, t] '[t, f]
sequence = toRight ||| traverse ||| fromLeft
