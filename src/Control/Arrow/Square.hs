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
import Data.Functor.Compose.List
import Data.Profunctor
import Data.Profunctor.Composition.List
import qualified Control.Arrow as A

-- |
-- > +-----+
-- > |     |
-- > |  @--a
-- > |     |
-- > +-----+
arr :: A.Arrow a => Square '[] '[a] '[] '[]
arr = Square (P . A.arr . dimap unId Id . unHom)

-- |
-- > +-----+
-- > a--\  |
-- > |  @--a
-- > a--/  |
-- > +-----+
(>>>) :: A.Arrow a => Square '[a, a] '[a] '[] '[]
(>>>) = Square (\(PComp p (P q)) -> P (A.arr Id A.<<< q A.<<< p A.<<< A.arr unId))

-- |
-- > +-_⊗d-+
-- > |  v  |
-- > a--@--a
-- > |  v  |
-- > +-_⊗d-+
second :: A.Arrow a => Square '[a] '[a] '[(,) d] '[(,) d]
second = Square (P . (A.>>> A.arr F) . (A.<<< A.arr unF) . A.second . unP)

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
right :: A.ArrowChoice a => Square '[a] '[a] '[Either d] '[Either d]
right = Square (P . (A.>>> A.arr F) . (A.<<< A.arr unF) . A.right . unP)

-- |
-- > H²-⊕--H
-- > |  v  |
-- > a²-@--a
-- > |  v  |
-- > H²-⊕--H
(+++) :: A.ArrowChoice a => Square21 '[a] '[a] '[a] Either Either
(+++) = Square $ \(P p1 :**: P p2) -> P (A.arr UncurryF A.<<< p1 A.+++ p2 A.<<< A.arr curryF)
