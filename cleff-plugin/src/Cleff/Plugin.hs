module Cleff.Plugin (plugin) where

import           Cleff.Plugin.Internal (Plugin, makePlugin)

plugin :: Plugin
plugin = makePlugin [("cleff", "Cleff.Internal.Rec", "Elem")]
