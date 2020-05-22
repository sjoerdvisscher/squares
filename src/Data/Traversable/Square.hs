{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
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
import Data.Functor.Compose.List
import Data.Profunctor.Composition.List
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
traverse = Square \(P (Star afb)) -> P (Star (fmap F . T.traverse afb . unF))

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
