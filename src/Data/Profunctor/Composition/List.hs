{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, FlexibleInstances, FlexibleContexts #-}
module Data.Profunctor.Composition.List where

import Data.Profunctor
import qualified Data.Profunctor.Composition as P
import Data.Type.List

data PList (ps :: [* -> * -> *]) (a :: *) (b :: *) where
  Hom :: (a -> b) -> PList '[] a b
  P :: p a b -> PList '[p] a b
  Procompose :: p a x -> PList (q ': qs) x b -> PList (p ': q ': qs) a b

instance Profunctor (PList '[]) where
  dimap l r (Hom f) = Hom (r . f . l)
instance Profunctor p => Profunctor (PList '[p]) where
  dimap l r (P p) = P (dimap l r p)
instance (Profunctor p, Profunctor (PList (q ': qs))) => Profunctor (PList (p ': q ': qs)) where
  dimap l r (Procompose p ps) = Procompose (lmap l p) (rmap r ps)

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
