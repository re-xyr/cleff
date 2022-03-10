{-# LANGUAGE CPP #-}
-- | Copyright: (c) 2022 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.Plugin (plugin) where

import           Cleff.Plugin.Internal (Plugin, makePlugin)

-- | The GHC typechecker plugin that disambiguates trivial uses of @cleff@ effects. Refer to the README for more info.
plugin :: Plugin
#if MIN_VERSION_cleff(0, 3, 2)
plugin = makePlugin [("cleff", "Cleff.Internal.Rec", ":>")]
#elif MIN_VERSION_cleff(0, 3, 1)
plugin = makePlugin [("cleff", "Cleff.Internal.Rec", "Elem")]
#elif MIN_VERSION_cleff(0, 2, 0)
plugin = makePlugin [("rec-smallarray", "Data.Rec.SmallArray", "Elem")]
#else
plugin = makePlugin [("cleff", "Data.Rec", "Elem")]
#endif
