{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Control.Arrow.Square where

import Data.Square
import Data.Profunctor
import Data.Profunctor.Composition
import Data.Profunctor.Composition.List
import qualified Control.Arrow as A

-- |
-- > +-----+
-- > |     |
-- > |  @--a
-- > |     |
-- > +-----+
arr :: (A.Arrow a, Profunctor a) => Square '[] '[a] '[] '[]
arr = mkSquare A.arr

-- |
-- > +-----+
-- > a--\  |
-- > |  @--a
-- > a--/  |
-- > +-----+
(>>>) :: (A.Arrow a, Profunctor a) => Square '[a, a] '[a] '[] '[]
(>>>) = mkSquare (\(Procompose q p) -> q A.<<< p)

-- |
-- > +-_⊗d-+
-- > |  v  |
-- > a--@--a
-- > |  v  |
-- > +-_⊗d-+
second :: (A.Arrow a, Profunctor a) => Square '[a] '[a] '[(,) d] '[(,) d]
second = mkSquare A.second

-- |
-- > H²-⊗--H
-- > |  v  |
-- > a²-@--a
-- > |  v  |
-- > H²-⊗--H
(***) :: A.Arrow a => Square21 '[a] '[a] '[a] (,) (,)
(***) = Square $ \(P p1 :**: P p2) -> P (A.arr UncurryF A.<<< p1 A.*** p2 A.<<< A.arr curryF)

-- |
-- > +-_⊕d-+
-- > |  v  |
-- > a--@--a
-- > |  v  |
-- > +-_⊕d-+
right :: (A.ArrowChoice a, Profunctor a) => Square '[a] '[a] '[Either d] '[Either d]
right = mkSquare A.right

-- |
-- > H²-⊕--H
-- > |  v  |
-- > a²-@--a
-- > |  v  |
-- > H²-⊕--H
(+++) :: A.ArrowChoice a => Square21 '[a] '[a] '[a] Either Either
(+++) = Square $ \(P p1 :**: P p2) -> P (A.arr UncurryF A.<<< p1 A.+++ p2 A.<<< A.arr curryF)
