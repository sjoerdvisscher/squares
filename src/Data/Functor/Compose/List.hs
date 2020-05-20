{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, FlexibleInstances, FlexibleContexts #-}
module Data.Functor.Compose.List where

import Data.Type.List

-- FList '[f, g, h] = h (g (f a))
data FList (fs :: [* -> *]) (a :: *) where
  Id :: { unId :: a } -> FList '[] a
  F :: { unF :: f a } -> FList '[f] a
  Compose :: { getCompose :: FList (g ': gs) (f a) } -> FList (f ': g ': gs) a

instance Functor (FList '[]) where
  fmap f = Id . f . unId
instance Functor f => Functor (FList '[f]) where
  fmap f = F . fmap f . unF
instance (Functor f, Functor (FList (g ': gs))) => Functor (FList (f ': g ': gs)) where
  fmap f = Compose . fmap (fmap f) . getCompose

class FAppend f where
  fappend :: Functor (FList g) => FList g (FList f a) -> FList (f ++ g) a
  funappend :: Functor (FList g) => FList (f ++ g) a -> FList g (FList f a)
instance FAppend '[] where
  fappend = fmap unId
  funappend = fmap Id
instance FAppend '[f] where
  fappend (Id fa) = F (unF fa)
  fappend f@F{} = Compose $ fmap unF f
  fappend f@Compose{} = Compose $ fmap unF f
  funappend fa@F{} = Id fa
  funappend (Compose fga@F{}) = fmap F fga
  funappend (Compose fga@Compose{}) = fmap F fga
instance (Functor f, FAppend (g ': gs)) => FAppend (f ': g ': gs) where
  fappend = Compose . fappend . fmap getCompose
  funappend = fmap Compose . funappend . getCompose
