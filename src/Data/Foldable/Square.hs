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

-- |
-- > +--f--+
-- > |  v  |
-- > !m-@-!m
-- > |  ?  |
-- > +--?--+
--
-- `F.foldMap` as a square. Note that because `Forget` ignores its output parameter,
-- this square can have any list of functors as output type.
foldMap :: (Foldable f, Monoid m, IsFList gs) => Square '[Forget m] '[Forget m] '[f] gs
foldMap = mkSquare (Forget . F.foldMap . runForget)
