{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

instance Profunctor (PList '[]) where
  dimap l r (Hom f) = Hom (r . f . l)
instance Profunctor p => Profunctor (PList '[p]) where
  dimap l r (P p) = P (dimap l r p)
instance (Profunctor p, Profunctor (PList (q ': qs))) => Profunctor (PList (p ': q ': qs)) where
  dimap l r (PComp p ps) = PComp (lmap l p) (rmap r ps)

-- | Combining and splitting nested `PList`s.
class PAppend p where
  pappend :: Profunctor (PList q) => Procompose (PList q) (PList p) a b -> PList (p ++ q) a b
  punappend :: PList (p ++ q) a b -> Procompose (PList q) (PList p) a b
instance PAppend '[] where
  pappend (Procompose q (Hom f)) = lmap f q
  punappend q = Procompose q (Hom id)
instance Profunctor p => PAppend '[p] where
  pappend (Procompose (Hom f) (P p)) = P (rmap f p)
  pappend (Procompose q@P{} (P p)) = PComp p q
  pappend (Procompose q@PComp{} (P p)) = PComp p q
  punappend p@P{} = Procompose (Hom id) p
  punappend (PComp p qs) = Procompose qs (P p)
instance (Profunctor p, PAppend (q ': qs)) => PAppend (p ': q ': qs) where
  pappend (Procompose q (PComp p ps)) = PComp p (pappend (Procompose q ps))
  punappend (PComp p pq) = case punappend pq of Procompose q ps -> Procompose q (PComp p ps)
