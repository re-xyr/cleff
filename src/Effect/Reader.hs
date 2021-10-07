module Effect.Reader where

import           Data.Typeable (Typeable)
import           Effect

data Reader r :: Effect where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

ask :: Reader r :> es => Eff es r
ask = send Ask

asks :: Reader r :> es => (r -> s) -> Eff es s
asks = (<$> ask)

local :: Reader r :> es => (r -> r) -> Eff es a -> Eff es a
local f = send . Local f

runReader :: forall r es a. Typeable r => r -> Eff (Reader r ': es) a -> Eff es a
runReader r = interpret \case
  Ask        -> pure r
  Local f m' -> runReader (f r) $ unlift m'
