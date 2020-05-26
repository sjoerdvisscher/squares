{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Rep.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Functor.Rep.Square where

import Data.Square
import qualified Data.Functor.Rep as R

-- |
-- > +-k→_-+
-- > |  v  |
-- > |  @  |
-- > |  v  |
-- > +--f--+
tabulate :: R.Representable f => Square '[] '[] '[(->) (R.Rep f)] '[f]
tabulate = funNat R.tabulate

-- |
-- > +--f--+
-- > |  v  |
-- > |  @  |
-- > |  v  |
-- > +-k→_-+
index :: R.Representable f => Square '[] '[] '[f] '[(->) (R.Rep f)]
index = funNat R.index
