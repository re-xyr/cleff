{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

#ifdef CLEFF_PLUGIN_cleff
import qualified CleffSpec
#endif
#ifdef CLEFF_PLUGIN_effectful
import qualified EffectfulSpec
#endif

main :: IO ()
main = putStrLn "It compiles!"
