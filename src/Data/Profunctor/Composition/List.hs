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
import qualified Data.Profunctor.Composition as P
import Data.Type.List

-- | N-ary composition of profunctors.
data PList (ps :: [* -> * -> *]) (a :: *) (b :: *) where
  Hom :: { unHom :: a -> b } -> PList '[] a b
  P :: { unP :: p a b } -> PList '[p] a b
  Procompose :: p a x -> PList (q ': qs) x b -> PList (p ': q ': qs) a b

instance Profunctor (PList '[]) where
  dimap l r (Hom f) = Hom (r . f . l)
instance Profunctor p => Profunctor (PList '[p]) where
  dimap l r (P p) = P (dimap l r p)
instance (Profunctor p, Profunctor (PList (q ': qs))) => Profunctor (PList (p ': q ': qs)) where
  dimap l r (Procompose p ps) = Procompose (lmap l p) (rmap r ps)

-- | Combining and splitting nested `PList`s.
class PAppend p where
  pappend :: Profunctor (PList q) => P.Procompose (PList q) (PList p) a b -> PList (p ++ q) a b
  punappend :: PList (p ++ q) a b -> P.Procompose (PList q) (PList p) a b
instance PAppend '[] where
  pappend (P.Procompose q (Hom f)) = lmap f q
  punappend q = P.Procompose q (Hom id)
instance Profunctor p => PAppend '[p] where
  pappend (P.Procompose (Hom f) (P p)) = P (rmap f p)
  pappend (P.Procompose q@P{} (P p)) = Procompose p q
  pappend (P.Procompose q@Procompose{} (P p)) = Procompose p q
  punappend p@P{} = P.Procompose (Hom id) p
  punappend (Procompose p qs) = P.Procompose qs (P p)
instance (Profunctor p, PAppend (q ': qs)) => PAppend (p ': q ': qs) where
  pappend (P.Procompose q (Procompose p ps)) = Procompose p (pappend (P.Procompose q ps))
  punappend (Procompose p pq) = case punappend pq of P.Procompose q ps -> P.Procompose q (Procompose p ps)
