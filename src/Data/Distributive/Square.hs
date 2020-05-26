{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Distributive.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Distributive.Square where

import Data.Square
import Data.Profunctor
import qualified Data.Distributive as D

-- |
-- > +--t--+
-- > |  v  |
-- > f<-@-<f
-- > |  v  |
-- > +--t--+
--
-- @cotraverse = (funId ||| fromRight) === distribute === (toLeft ||| funId)@
cotraverse :: (D.Distributive t, Functor f) => Square '[Costar f] '[Costar f] '[t] '[t]
cotraverse = mkSquare (Costar . D.cotraverse . runCostar)

-- |
-- > +---t-f-+
-- > |   v v |
-- > | /-@-/ |
-- > | v v   |
-- > +-f-t---+
--
-- @distribute = fromRight ||| cotraverse ||| toLeft@
distribute :: (D.Distributive t, Functor f) => Square '[] '[] '[t, f] '[f, t]
distribute = fromRight ||| cotraverse ||| toLeft
