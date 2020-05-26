{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Control.Comonad.Square where

import Data.Square
import Data.Profunctor
import qualified Control.Comonad as W

-- |
-- > +--w--+
-- > |  v  |
-- > |  X  |
-- > |     |
-- > +-----+
extract :: W.Comonad w => Square '[] '[] '[w] '[]
extract = mkSquare (. W.extract)

-- |
-- > +--w--+
-- > |  v  |
-- > w<-E  |
-- > |  v  |
-- > +--w--+
--
-- `W.extend` as a square
--
-- Right identity law:
--
-- > +--w--+
-- > |  v  |     +--w--+
-- > w<-E  |     |  v  |
-- > |  v  | === w<-/  |
-- > |  X  |     |     |
-- > +-----+     +-----+
--
-- Left identity law:
--
-- > +---w-+
-- > |   v |     +--w--+
-- > | /-E |     |  |  |
-- > | v | | === |  v  |
-- > | X v |     |  |  |
-- > +---w-+     +--w--+
--
-- Associativity law:
--
-- > +--w--+     +---w-+
-- > |  v  |     |   v |
-- > w<-E  |     | /-E |
-- > |  v  | === w<E | |
-- > w<-E  |     | | | |
-- > |  v  |     w</ v |
-- > +--w--+     +---w-+
extend :: W.Comonad w => Square '[Costar w] '[] '[w] '[w]
extend = mkSquare (W.extend . runCostar)

-- |
-- > +---w-+
-- > |   v |
-- > | /-@ |
-- > | v v |
-- > +-w-w-+
--
-- > duplicate = fromRight ||| extend
duplicate :: W.Comonad w => Square '[] '[] '[w] '[w, w]
duplicate = fromRight ||| extend

-- |
-- > +-----+
-- > |  /-<w
-- > w<-@  |
-- > w<-/  |
-- > +-----+
--
-- Cokleisli composition `(W.<=<)`
--
-- > (<=<) = fromRight === extend === toLeft
(<=<) :: W.Comonad w => Square '[Costar w, Costar w] '[Costar w] '[] '[]
(<=<) = fromRight === extend === toLeft
