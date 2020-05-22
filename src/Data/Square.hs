{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------

module Data.Square where

import Data.Functor.Compose.List
import Data.Profunctor
import qualified Data.Profunctor.Composition as P
import Data.Profunctor.Composition.List
import Data.Type.List
#if __GLASGOW_HASKELL__ >= 810
import Data.Kind
#endif

-- * Double category

-- $doubleCategory
-- There is a double category of Haskell functors and profunctors.
--
-- The squares in this double category are natural transformations.

-- |
-- > +-----+
-- > |     |
-- > |     |
-- > |     |
-- > +-----+
--
-- > forall a b. (a -> b) -> (a -> b)
--
-- The empty square is the identity transformation.
emptySquare :: Square '[] '[] '[] '[]
emptySquare = Square $ dimap unId Id

-- |
-- > +-----+
-- > |     |
-- > p-----p
-- > |     |
-- > +-----+
--
-- > forall a b. p a b -> p a b
--
-- Profunctors are drawn as horizontal lines.
--
-- Note that `emptySquare` is `proId` for the profunctor @(->)@.
-- We don't draw a line for @(->)@ because it is the identity for profunctor composition.
proId :: Profunctor p => Square '[p] '[p] '[] '[]
proId = Square $ dimap unId Id

-- |
-- > +--f--+
-- > |  |  |
-- > |  v  |
-- > |  |  |
-- > +--f--+
--
-- > forall a b. (a -> b) -> (f a -> f b)
--
-- Functors are drawn with vertical lines with arrow heads.
-- You will recognize the above type as `fmap`!
--
-- We don't draw lines for the identity functor, because it is the identity for functor composition.
funId :: Functor f => Square '[] '[] '[f] '[f]
funId = Square \(Hom f) -> Hom (fmap f)

-- |
-- > +--f--+
-- > |  |  |
-- > |  @  |
-- > |  |  |
-- > +--g--+
--
-- > forall a b. (a -> b) -> (f a -> g b)
--
-- Non-identity transformations are drawn with an @\@@ in the middle.
-- Natural transformations between haskell functors are usualy given the type
-- @forall a. f a -> g a@. The type above you get when `fmap`ping before or after.
-- (It doesn't matter which, because of naturality!)
funNat :: (Functor f, Functor g) => (f ~> g) -> Square '[] '[] '[f] '[g]
funNat n = Square $ Hom . dimap unF F . (.) n . fmap . unHom

-- |
-- > +-----+
-- > |     |
-- > p--@--q
-- > |     |
-- > +-----+
--
-- > forall a b. p a b -> q a b
--
-- Natural transformations between profunctors.
proNat :: (Profunctor p, Profunctor q) => (p :-> q) -> Square '[p] '[q] '[] '[]
proNat n = Square $ P . dimap unId Id . n . unP

-- |
-- > +--f--+
-- > |  v  |
-- > p--@--q
-- > |  v  |
-- > +--g--+
--
-- > forall a b. p a b -> q (f a) (g b)
--
-- The complete definition of a square is a combination of natural transformations
-- between functors and natural transformations between profunctors.
--
-- To make type inferencing easier the above type is wrapped by a newtype.
#if __GLASGOW_HASKELL__ >= 810
type SquareNT :: (a -> b -> Type) -> (c -> d -> Type) -> (a -> c) -> (b -> d) -> Type
#endif
newtype SquareNT p q f g = Square { unSquare :: forall a b. p a b -> q (f a) (g b) }

-- | To make composing squares associative, this library uses squares with lists of functors and profunctors,
-- which are composed together.
--
-- > FList '[] a ~ a
-- > FList '[f, g, h] a ~ h (g (f a))
-- > PList '[] a b ~ a -> b
-- > PList '[p, q, r] a b ~ (p a x, q x y, r y b)
type Square ps qs fs gs = SquareNT (PList ps) (PList qs) (FList fs) (FList gs)

-- | A helper function to add the wrappers needed for `PList` and `FList`,
-- if the square has exactly one (pro)functor on each side (which is common).
mkSquare :: Profunctor q => (forall a b. p a b -> q (f a) (g b)) -> Square '[p] '[q] '[f] '[g]
mkSquare f = Square (P . dimap unF F . f . unP)

-- |
-- > +--f--+     +--h--+       +--f--h--+
-- > |  v  |     |  v  |       |  v  v  |
-- > p--@--q ||| q--@--r  ==>  p--@--@--r
-- > |  v  |     |  v  |       |  v  v  |
-- > +--g--+     +--i--+       +--g--i--+
--
-- Horizontal composition of squares. `proId` is the identity of `(|||)`.
infixl 6 |||
(|||) :: (Profunctor (PList rs), FAppend fs, FAppend gs, Functor (FList hs), Functor (FList is))
      => Square ps qs fs gs -> Square qs rs hs is -> Square ps rs (fs ++ hs) (gs ++ is) -- ^
Square pq ||| Square qr = Square $ dimap funappend fappend . qr . pq

-- |
-- > +--f--+
-- > |  v  |
-- > p--@--q              +--f--+
-- > |  v  |              |  v  |
-- > +--g--+              p--@--q
-- >   ===        ==>     |  v  |
-- > +--g--+              r--@--s
-- > |  v  |              |  v  |
-- > r--@--s              +--h--+
-- > |  v  |
-- > +--h--+
--
-- Vertical composition of squares. `funId` is the identity of `(===)`.
infixl 5 ===
(===) :: (PAppend ps, PAppend qs, Profunctor (PList ss))
      => Square ps qs fs gs -> Square rs ss gs hs -> Square (ps ++ rs) (qs ++ ss) fs hs -- ^
Square pq === Square rs = Square \pr -> case punappend pr of P.Procompose r p -> pappend (P.Procompose (rs r) (pq p))


-- * Proarrow equipment
--
-- $proarrowEquipment
-- The double category of haskell functors and profunctors is a proarrow equipment.
-- Which means that we can "bend" functors to become profunctors.

-- |
-- > +--f--+
-- > |  v  |
-- > |  \->f
-- > |     |
-- > +-----+
--
-- A functor @f@ can be bent to the right to become the profunctor @`Star` f@.
toRight :: Functor f => Square '[] '[Star f] '[f] '[]
toRight = Square \(Hom f) -> P (Star (fmap (Id . f) . unF))

-- |
-- > +--f--+
-- > |  v  |
-- > f<-/  |
-- > |     |
-- > +-----+
--
-- A functor @f@ can be bent to the left to become the profunctor @`Costar` f@.
toLeft :: Square '[Costar f] '[] '[f] '[]
toLeft = Square \(P (Costar f)) -> Hom (Id . f . unF)

-- |
-- > +-----+
-- > |     |
-- > |  /-<f
-- > |  v  |
-- > +--f--+
--
-- The profunctor @`Costar` f@ can be bent down to become the functor @f@ again.
fromRight :: Functor f => Square '[] '[Costar f] '[] '[f]
fromRight = Square \(Hom f) -> P (Costar (F . fmap (f . unId)))

-- |
-- > +-----+
-- > |     |
-- > f>-\  |
-- > |  v  |
-- > +--f--+
--
-- The profunctor @`Star` f@ can be bent down to become the functor @f@ again.
fromLeft :: Square '[Star f] '[] '[] '[f]
fromLeft = Square \(P (Star f)) -> Hom (F . f . unId)

-- |
-- > +-----+
-- > f>-\  |         fromLeft
-- > |  v  |         ===
-- > f<-/  |         toLeft
-- > +-----+
--
-- `fromLeft` and `toLeft` can be composed vertically to bend @`Star` f@ back to @`Costar` f@.
uLeft :: Functor f => Square '[Star f, Costar f] '[] '[] '[]
uLeft =
  fromLeft
  ===
  toLeft

-- |
-- > +-----+
-- > |  /-<f         fromRight
-- > |  v  |         ===
-- > |  \->f         toRight
-- > +-----+
--
-- `fromRight` and `toRight` can be composed vertically to bend @`Costar` f@ to @`Star` f@.
uRight :: Functor f => Square '[] '[Costar f, Star f] '[] '[]
uRight =
  fromRight
  ===
  toRight

-- |
-- > +f-f-f+     +--f--+     spiderLemma n =
-- > |v v v|     f> v <f       fromLeft ||| funId ||| fromRight
-- > | \|/ |     | \|/ |       ===
-- > p--@--q ==> p--@--q       n
-- > | /|\ |     | /|\ |       ===
-- > |v v v|     g< v >g       toLeft ||| funId ||| toRight
-- > +g-g-g+     +--g--+
--
-- The spider lemma is an example how bending wires can also be seen as sliding functors around corners.
spiderLemma :: (Profunctor p, Profunctor q, Functor f1, Functor f2, Functor f3, Functor g1, Functor g2, Functor g3)
  => Square '[p] '[q] '[f1, f2, f3] '[g1, g2, g3]
  -> Square '[Star f1, p, Costar g1] '[Costar f3, q, Star g3] '[f2] '[g2] -- ^
spiderLemma n =
  fromLeft ||| funId ||| fromRight
  ===
  n
  ===
  toLeft ||| funId ||| toRight

-- |> spiderLemma' n = (toRight === proId === fromRight) ||| n ||| (toLeft === proId === fromLeft)
--
-- The spider lemma in the other direction.
spiderLemma' :: (Profunctor p, Profunctor q, Functor f1, Functor f2, Functor f3, Functor g1, Functor g2, Functor g3)
  => Square '[Star f1, p, Costar g1] '[Costar f3, q, Star g3] '[f2] '[g2]
  -> Square '[p] '[q] '[f1, f2, f3] '[g1, g2, g3] -- ^
spiderLemma' n = (toRight === proId === fromRight) ||| n ||| (toLeft === proId === fromLeft)

-- * In other categories than Hask

-- $otherCategories
-- > A--f--C
-- > |  v  |
-- > p--@--q
-- > |  v  |
-- > B--g--D
--
-- Squares can be generalized further by choosing a different category for each quadrant.
-- To use this, `SquareNT` has been made kind polymorphic:
--
-- > type SquareNT :: (a -> b -> Type) -> (c -> d -> Type) -> (a -> c) -> (b -> d) -> Type
--
-- This library is mostly about staying in Hask, but it is interesting to use f.e. the
-- product category @Hask × Hask@ or the unit category.

-- |
-- > H²-f--H
-- > |  v  |
-- > p--@--q     H = Hask, H² = Hask x Hask
-- > |  v  |
-- > H²-g--H
--
type Square21 ps1 ps2 qs f g = SquareNT (PList ps1 :**: PList ps2) (PList qs) (UncurryF f) (UncurryF g)

-- | Combine two profunctors from Hask to a profunctor from Hask x Hask
data (p1 :**: p2) a b where
  (:**:) :: p1 a1 b1 -> p2 a2 b2 -> (p1 :**: p2) '(a1, a2) '(b1, b2)

-- | Uncurry the kind of a bifunctor.
--
-- > type UncurryF :: (a -> b -> Type) -> (a, b) -> Type
#if __GLASGOW_HASKELL__ >= 810
type UncurryF :: (a -> b -> Type) -> (a, b) -> Type
#endif
data UncurryF f a where
  UncurryF :: { curryF :: f a b } -> UncurryF f '(a, b)

-- |
-- > 1--a--H
-- > |  v  |
-- > U--@--q     1 = Hask^0 = (), H = Hask
-- > |  v  |
-- > 1--b--H
--
type Square01 q a b = SquareNT Unit (PList q) (ValueF a) (ValueF b)

-- | The boring profunctor from and to the unit category.
data Unit a b where
  Unit :: Unit '() '()

-- | Values as a functor from the unit category.
data ValueF x u where
  ValueF :: a -> ValueF a '()
