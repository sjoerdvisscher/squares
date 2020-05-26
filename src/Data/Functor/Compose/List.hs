{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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

-- | Calculate the simplified type of the composition of a list of functors.
type family PlainF (fs :: [* -> *]) (a :: *) :: *
type instance PlainF '[] a = a
type instance PlainF (f ': fs) a = PlainF fs (f a)

-- | Functions for working with `FList`s.
class IsFList fs where
  -- | Combine 2 nested `FList`s into one `FList`.
  fappend :: Functor (FList gs) => FList gs (FList fs a) -> FList (fs ++ gs) a
  -- | Split one `FList` into 2 nested `FList`s.
  funappend :: Functor (FList gs) => FList (fs ++ gs) a -> FList gs (FList fs a)
  -- | Convert an `FList` to its simplified form.
  toPlainF :: FList fs a -> PlainF fs a
  -- | Create an `FList` from its simplified form.
  fromPlainF :: PlainF fs a -> FList fs a
instance IsFList '[] where
  fappend = fmap unId
  funappend = fmap Id
  toPlainF (Id a) = a
  fromPlainF a = Id a
instance IsFList '[f] where
  fappend (Id fa) = F (unF fa)
  fappend f@F{} = FComp $ fmap unF f
  fappend f@FComp{} = FComp $ fmap unF f
  funappend fa@F{} = Id fa
  funappend (FComp fga@F{}) = fmap F fga
  funappend (FComp fga@FComp{}) = fmap F fga
  toPlainF (F fa) = fa
  fromPlainF fa = F fa
instance IsFList (g ': gs) => IsFList (f ': g ': gs) where
  fappend = FComp . fappend . fmap unFComp
  funappend = fmap FComp . funappend . unFComp
  toPlainF (FComp fgs) = toPlainF fgs
  fromPlainF fgs = FComp (fromPlainF fgs)


-- | Natural transformations between two functors. (Why is this still not in base??)
type f ~> g = forall a. f a -> g a
