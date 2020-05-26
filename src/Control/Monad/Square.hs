{-# LANGUAGE DataKinds #-}
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
import qualified Control.Monad as M

-- |
-- > +-----+
-- > |     |
-- > |  R  |
-- > |  v  |
-- > +--m--+
return :: Monad m => Square '[] '[] '[] '[m]
return = mkSquare (M.return .)

-- |
-- > +--m--+
-- > |  v  |
-- > m>-B  |
-- > |  v  |
-- > +--m--+
--
-- `(>>=)` as a square (or to be precise its flipped version `(=<<)`)
--
-- Left identity law:
--
-- > +-----+
-- > |  R  |     +-----+
-- > |  v  |     |     |
-- > m>-B  | === m>-\  |
-- > |  v  |     |  v  |
-- > +--m--+     +--m--+
--
-- Right identity law:
--
-- > +---m-+
-- > | R v |     +--m--+
-- > | v | |     |  |  |
-- > | \-B | === |  v  |
-- > |   v |     |  |  |
-- > +---m-+     +--m--+
--
-- Associativity law:
--
-- > +--m--+     +---m-+
-- > |  v  |     m>\ v |
-- > m>-B  |     | v | |
-- > |  v  | === m>B | |
-- > m>-B  |     | \-B |
-- > |  v  |     |   v |
-- > +--m--+     +---m-+
bind :: Monad m => Square '[Star m] '[] '[m] '[m]
bind = mkSquare (flip (>>=) . runStar)

-- |
-- > +-m-m-+
-- > | v v |
-- > | \-@ |
-- > |   v |
-- > +---m-+
--
-- > join = toRight ||| bind
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
--
-- > (>=>) = fromLeft === bind === toRight
(>=>) :: Monad m => Square '[Star m, Star m] '[Star m] '[] '[]
(>=>) = fromLeft === bind === toRight
