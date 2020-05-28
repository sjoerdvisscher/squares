{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Foldable.Square where

import Data.Square
import Data.Profunctor
import Data.Functor.Compose.List
import qualified Data.Foldable as F
import Control.Applicative

-- |
-- > +--t--+
-- > |  v  |
-- > !m-@-!m
-- > |  ?  |
-- > +--?--+
--
-- `F.foldMap` as a square. Note that because `Forget` ignores its output parameter,
-- this square can have any list of functors as output type.
foldMap :: (Foldable t, Monoid m, IsFList gs) => Square '[Forget m] '[Forget m] '[t] gs
foldMap = mkSquare (Forget . F.foldMap . runForget)

-- | `Data.Foldable.Square.any` is `Data.Foldable.Square.foldMap` specialized to `Data.Monoid.Any`.
any :: (Foldable t, IsFList gs) => Square '[Forget Bool] '[Forget Bool] '[t] gs
any = mkSquare (Forget . F.any . runForget)

-- | `Data.Foldable.Square.all` is `Data.Foldable.Square.foldMap` specialized to `Data.Monoid.All`.
all :: (Foldable t, IsFList gs) => Square '[Forget Bool] '[Forget Bool] '[t] gs
all = mkSquare (Forget . F.all . runForget)

-- |
-- > +--t--+
-- > |  v  |
-- > f>-@->f
-- > |     |
-- > +-----+
--
-- `afoldMap` is a mapping version of `F.asum`, or a generalization of `F.concatMap`.
afoldMap :: (Foldable t, Alternative f) => Square '[Star f] '[Star f] '[t] '[]
afoldMap = mkSquare (Star . (\f -> foldr ((<|>) . f) empty) . runStar)
