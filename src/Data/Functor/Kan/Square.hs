{-# LANGUAGE DataKinds, RankNTypes, GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Kan.Square
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  sjoerd@w3future.com
--
-----------------------------------------------------------------------------
module Data.Functor.Kan.Square where

import Data.Square
import Data.Profunctor
import Data.Profunctor.Composition
import Data.Functor.Compose.List
import Data.Functor.Kan.Lan
import Data.Functor.Kan.Ran

-- |
-- > +--f--+
-- > |  v  |
-- > |  @  |
-- > | / \ |
-- > | v v |
-- > +-j-L-+
lanSquare :: Functor f => Square '[] '[] '[f] '[j, Lan j f]
lanSquare = mkSquare $ \k -> glan . fmap k

-- |
-- > +--f--+     +--L--+
-- > |  v  |     |  v  |
-- > |  @  | ==> |  @  |
-- > | / \ |     |  |  |
-- > | v v |     |  v  |
-- > +-j-g-+     +--g--+
--
-- Any square like the one on the left factors through 'lanSquare'.
-- 'lanFactor' gives the remaining square.
lanFactor :: Functor g => Square '[] '[] '[f] '[j, g] -> Square '[] '[] '[Lan j f] '[g]
lanFactor sq = mkSquare $ \k -> fmap k . toLan (runSquare sq id)

-- |
-- > +-j-R-+
-- > | v v |
-- > | \ / |
-- > |  @  |
-- > |  v  |
-- > +--g--+
ranSquare :: Functor g => Square '[] '[] '[j, Ran j g] '[g]
ranSquare = mkSquare $ \k -> fmap k . gran

-- |
-- > +-j-f-+     +--f--+
-- > | v v |     |  v  |
-- > | \ / | ==> |  @  |
-- > |  @  |     |  |  |
-- > |  v  |     |  v  |
-- > +--g--+     +--R--+
--
-- Any square like the one on the left factors through 'ranSquare'.
-- 'ranFactor' gives the remaining square.
ranFactor :: Functor f => Square '[] '[] '[j, f] '[g] -> Square '[] '[] '[f] '[Ran j g]
ranFactor sq = mkSquare $ \k -> fmap k . toRan (runSquare sq id)