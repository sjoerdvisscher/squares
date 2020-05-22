{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Compose.List
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Functor.Compose.List where

import Data.Type.List

-- | N-ary composition of functors.
--
-- > FList '[] a ~ a
-- > FList '[f, g, h] a ~ h (g (f a))
data FList (fs :: [* -> *]) (a :: *) where
  Id :: { unId :: a } -> FList '[] a
  F :: { unF :: f a } -> FList '[f] a
  FComp :: { unFComp :: FList (g ': gs) (f a) } -> FList (f ': g ': gs) a

instance Functor (FList '[]) where
  fmap f = Id . f . unId
instance Functor f => Functor (FList '[f]) where
  fmap f = F . fmap f . unF
instance (Functor f, Functor (FList (g ': gs))) => Functor (FList (f ': g ': gs)) where
  fmap f = FComp . fmap (fmap f) . unFComp


-- | Combining and splitting nested `FList`s.
class FAppend f where
  fappend :: Functor (FList g) => FList g (FList f a) -> FList (f ++ g) a
  funappend :: Functor (FList g) => FList (f ++ g) a -> FList g (FList f a)
instance FAppend '[] where
  fappend = fmap unId
  funappend = fmap Id
instance FAppend '[f] where
  fappend (Id fa) = F (unF fa)
  fappend f@F{} = FComp $ fmap unF f
  fappend f@FComp{} = FComp $ fmap unF f
  funappend fa@F{} = Id fa
  funappend (FComp fga@F{}) = fmap F fga
  funappend (FComp fga@FComp{}) = fmap F fga
instance (Functor f, FAppend (g ': gs)) => FAppend (f ': g ': gs) where
  fappend = FComp . fappend . fmap unFComp
  funappend = fmap FComp . funappend . unFComp


-- | Natural transformations between two functors. (Why is this still not in base??)
type f ~> g = forall a. f a -> g a
