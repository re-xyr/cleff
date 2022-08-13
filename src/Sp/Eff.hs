module Sp.Eff
  ( -- * Basic operations
    Effect
  , Eff
  , (:>)
    -- ** Performing effects
  , send
    -- ** Trivial transformations
  , lift
  , runEff
    -- * Effect handling
  , Handling
  , Handler
    -- ** Providing handlers
  , interpret
  , reinterpret
  , interpose
  , reinterpose
    -- ** Combinators to use in handlers
  , embed
  , withUnembed
  , abort
  , control
  , Localized
    -- * @IO@ support
  , IOE
  , runIOE
  ) where

import           Sp.Internal.Monad
