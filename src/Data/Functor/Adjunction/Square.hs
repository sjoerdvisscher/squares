{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Adjunction.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Functor.Adjunction.Square where

import Data.Square
import Data.Profunctor
import qualified Data.Functor.Adjunction as A

-- |
-- > +-----+
-- > |     |
-- > f<-@->g
-- > |     |
-- > +-----+
--
-- > leftAdjunct = unit === (toLeft ||| toRight)
leftAdjunct :: A.Adjunction f g => Square '[Costar f] '[Star g] '[] '[]
leftAdjunct = mkSquare (Star . A.leftAdjunct . runCostar)

-- |
-- > +-----+
-- > |     |
-- > g>-@-<f
-- > |     |
-- > +-----+
--
-- > rightAdjunct = (fromLeft ||| fromRight) === counit
rightAdjunct :: A.Adjunction f g => Square '[Star g] '[Costar f] '[] '[]
rightAdjunct = mkSquare (Costar . A.rightAdjunct . runStar)

-- |
-- > +-----+
-- > |     |
-- > | /@\ |
-- > | v v |
-- > +-f-g-+
--
-- > unit = fromRight ||| leftAdj ||| fromLeft
unit :: A.Adjunction f g => Square '[] '[] '[] '[f, g]
unit = mkSquare (A.unit .)

-- |
-- > +-g-f-+
-- > | v v |
-- > | \@/ |
-- > |     |
-- > +-----+
--
-- > counit = toRight ||| rightAdjoint ||| toLeft
counit :: A.Adjunction f g => Square '[] '[] '[g, f] '[]
counit = mkSquare (. A.counit)
