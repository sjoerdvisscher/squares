{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Profunctor.Composition.List
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Profunctor.Composition.List where

import Data.Profunctor
import Data.Profunctor.Composition
import Data.Type.List

-- | N-ary composition of profunctors.
data PList (ps :: [* -> * -> *]) (a :: *) (b :: *) where
  Hom :: { unHom :: a -> b } -> PList '[] a b
  P :: { unP :: p a b } -> PList '[p] a b
  PComp :: p a x -> PList (q ': qs) x b -> PList (p ': q ': qs) a b

#if __GLASGOW_HASKELL__ >= 806
instance Functor (PList '[] a) where
  fmap f (Hom ab) = Hom (f . ab)
instance (forall a. Functor (p a)) => Functor (PList '[p] a) where
  fmap f (P p) = P (fmap f p)
instance (forall a. Functor (PList (q ': qs) a)) => Functor (PList (p ': q ': qs) a) where
  fmap f (PComp p ps) = PComp p (fmap f ps)
#endif

instance Profunctor (PList '[]) where
  dimap l r (Hom f) = Hom (r . f . l)
instance Profunctor p => Profunctor (PList '[p]) where
  dimap l r (P p) = P (dimap l r p)
instance (Profunctor p, Profunctor (PList (q ': qs))) => Profunctor (PList (p ': q ': qs)) where
  dimap l r (PComp p ps) = PComp (lmap l p) (rmap r ps)

-- | Calculate the simplified type of the composition of a list of profunctors.
type family PlainP (ps :: [* -> * -> *]) :: * -> * -> *
type instance PlainP '[] = (->)
type instance PlainP '[p] = p
type instance PlainP (p ': q ': qs) = Procompose (PlainP (q ': qs)) p

-- | Functions for working with `PList`s.
class IsPList ps where
  -- | Combine 2 nested `PList`s into one `PList`.
  pappend :: (Profunctor (PList ps), Profunctor (PList qs)) => Procompose (PList qs) (PList ps) :-> PList (ps ++ qs)
  -- | Split one `PList` into 2 nested `PList`s.
  punappend :: PList (ps ++ qs) :-> Procompose (PList qs) (PList ps)
  -- | Convert a `PList` to its simplified form.
  toPlainP :: PList ps :-> PlainP ps
  -- | Create a `PList` from its simplified form.
  fromPlainP :: PlainP ps :-> PList ps
instance IsPList '[] where
  pappend (Procompose q (Hom f)) = lmap f q
  punappend q = Procompose q (Hom id)
  toPlainP (Hom f) = f
  fromPlainP f = Hom f
instance IsPList '[p] where
  pappend (Procompose (Hom f) p) = rmap f p
  pappend (Procompose q@P{} (P p)) = PComp p q
  pappend (Procompose q@PComp{} (P p)) = PComp p q
  punappend p@P{} = Procompose (Hom id) p
  punappend (PComp p qs) = Procompose qs (P p)
  toPlainP (P p) = p
  fromPlainP p = P p
instance (Profunctor (PList (q ': qs)), IsPList (q ': qs)) => IsPList (p ': q ': qs) where
  pappend (Procompose q (PComp p ps)) = PComp p (pappend (Procompose q ps))
  punappend (PComp p pq) = case punappend pq of Procompose q ps -> Procompose q (PComp p ps)
  toPlainP (PComp p pq) = Procompose (toPlainP pq) p
  fromPlainP (Procompose pq p) = PComp p (fromPlainP pq)
