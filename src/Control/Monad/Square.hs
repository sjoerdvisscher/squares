{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Control.Monad.Square where

import Prelude hiding (return)
import Data.Square
import Data.Profunctor
import Data.Functor.Compose.List
import Data.Profunctor.Composition.List
import qualified Control.Monad as M (return, (>=>))

-- |
-- > +-----+
-- > |     |
-- > |  R->m
-- > |     |
-- > +-----+
return :: Monad m => Square '[] '[Star m] '[] '[]
return = Square \(Hom f) -> P (Star (M.return . Id . f . unId))

-- |
-- > +--m--+
-- > |  v  |
-- > m>-B  |
-- > |  v  |
-- > +--m--+
--
-- `(>>=)`
--
-- Left identity law:
--
-- > +-------+
-- > | R>-\  +     +-----+
-- > |    v  |     |     |
-- > m>---B  | === m>-\  |
-- > |    v  |     |  v  |
-- > +----m--+     +--m--+
--
-- Right identity law:
--
-- > +----m--+     +--m--+
-- > |    v  |     |  |  |
-- > | R>-B  | === |  v  |
-- > |    v  |     |  |  |
-- > +----m--+     +--m--+
--
-- Associativity law:
--
-- > +--m--+     +-----m--+
-- > |  v  |     m>-\  v  |
-- > m>-B  |     |  v  |  |
-- > |  v  | === m>-B  |  |
-- > m>-B  |     |  \->B  |
-- > |  v  |     |     v  |
-- > +--m--+     +-----m--+
bind :: Monad m => Square '[Star m] '[] '[m] '[m]
bind = Square \(P (Star amb)) -> Hom \(F ma) -> F (ma >>= amb)

-- |
-- > +-m-m-+
-- > | v v |
-- > | \-@ |
-- > |   v |
-- > +---m-+
--
-- @join = toRight ||| bind@
join :: Monad m => Square '[] '[] '[m, m] '[m]
join = toRight ||| bind

-- |
-- > +-----+
-- > m>-\  |
-- > m>-@  |
-- > |  \->m
-- > +-----+
--
-- Kleisli composition `(M.>=>)`
kleisli :: Monad m => Square '[Star m, Star m] '[Star m] '[] '[]
kleisli = fromLeft === bind === toRight
