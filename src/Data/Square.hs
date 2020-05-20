{-# LANGUAGE GADTs, RankNTypes, DataKinds, PolyKinds, TypeOperators, FlexibleContexts, CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

module Data.Square where

import Data.Functor.Compose.List
import Data.Profunctor
import qualified Data.Profunctor.Composition as P
import Data.Profunctor.Composition.List
import Data.Type.List
#if __GLASGOW_HASKELL__ >= 810
import Data.Kind
#endif

-- |@
-- A--f--B
-- |  v  |
-- p--\@--q
-- |  v  |
-- C--g--D
-- @
#if __GLASGOW_HASKELL__ >= 810
type GSquare :: (a -> b -> Type) -> (c -> d -> Type) -> (a -> c) -> (b -> d) -> Type
#endif
type GSquare p q f g = forall a b. p a b -> q (f a) (g b)

type Square p q f g = GSquare (PList p) (PList q) (FList f) (FList g)


-- |@
-- +-----+
-- |     |
-- p-----p
-- |     |
-- +-----+
-- @
type Pro p = Square '[p] '[p] '[] '[]
idPro :: Profunctor p => Pro p
idPro = dimap unId Id

-- |@
-- +--f--+
-- |  |  |
-- |  v  |
-- |  |  |
-- +--f--+
-- @
type Fun f = Square '[] '[] '[f] '[f]
idArr :: Functor f => Fun f
idArr (Hom f) = Hom (fmap f)

-- |@
-- +--f--+     +--h--+       +-f-h-+
-- |  v  |     |  v  |       | v v |
-- p--\@--q ||| q--\@--r  ==>  p-\@-\@-r
-- |  v  |     |  v  |       | v v |
-- +--g--+     +--i--+       +-g-i-+
-- @
infixl 6 |||
(|||) :: (Profunctor (PList r), FAppend f, FAppend g, Functor (FList h), Functor (FList i))
      => Square p q f g -> Square q r h i -> Square p r (f ++ h) (g ++ i) -- ^
pq ||| qr = dimap funappend fappend . qr . pq

-- |@
-- +--f--+
-- |  v  |
-- p--\@--q
-- |  v  |
-- +--g--+
--   ===
-- +--g--+
-- |  v  |
-- r--\@--s
-- |  v  |
-- +--h--+
--
--   ==>
--
-- +--f--+
-- |  v  |
-- p--\@--q
-- |  v  |
-- r--\@--s
-- |  v  |
-- +--h--+
-- @
infixl 5 ===
(===) :: (PAppend p, PAppend q, Profunctor (PList s)) => Square p q f g -> Square r s g h -> Square (p ++ r) (q ++ s) f h
(pq === rs) pr = case punappend pr of P.Procompose r p -> pappend (P.Procompose (rs r) (pq p))


-- | Combine two profunctors from Hask to a profunctor from Hask x Hask
data (p1 :**: p2) a b where
  (:**:) :: p1 a1 b1 -> p2 a2 b2 -> (p1 :**: p2) '(a1, a2) '(b1, b2)

data UncurryF f a where
  UncurryF :: { curryF :: f a b } -> UncurryF f '(a, b)

-- |@
-- 2--f--1
-- |  v  |
-- p--\@--q     1 = Hask, 2 = Hask x Hask
-- |  v  |
-- 2--g--1
-- @
type Square21 p1 p2 q f g = GSquare (PList p1 :**: PList p2) (PList q) (UncurryF f) (UncurryF g)

data Unit a b where
  Unit :: Unit '() '()

data ValueF x u where
  ValueF :: a -> ValueF a '()

-- |@
-- 0--a--1
-- |  v  |
-- U--\@--q     0 = Hask^0 = (), 1 = Hask
-- |  v  |
-- 0--b--1
-- @
type Square01 q a b = GSquare Unit (PList q) (ValueF a) (ValueF b)
