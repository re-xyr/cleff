module Context where

import           Cleff
import           Cleff.Error
import           Cleff.Reader

data SomeEff :: Effect where
  SomeAction :: SomeEff m String
makeEffect ''SomeEff

data Ex = Ex deriving (Eq, Show)

good1 :: Either Ex String
good1 = runPure $ runError @Ex $ interpret (\SomeAction -> throwError Ex) do
  someAction `catchError` \Ex -> return "caught"

-- >>> good1
-- Right "caught"

good2 :: String
good2 = runPure $ runReader "unlocaled" $ interpret (\SomeAction -> ask) do
  local (const "localed") someAction

-- >>> good2
-- "unlocaled"
