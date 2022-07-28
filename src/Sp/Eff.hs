module Sp.Eff
  ( Eff
  , Handling
  , Handler
  , lift
  , interpret
  , reinterpret
  , interpose
  , reinterpose
  , send
  , toEff
  , control
  , abort
  , runEff
  , (:>)
  ) where

import           Sp.Internal.Monad
