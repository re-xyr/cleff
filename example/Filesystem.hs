-- | This module is adapted from https://github.com/arybczak/effectful/blob/master/effectful/examples/FileSystem.hs,
-- originally BSD3 license, authors Andrzej Rybczak et al.
module Filesystem where

import           Cleff
import           Cleff.Error
import           Cleff.State
import           Control.Monad.Extra    (maybeM)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import qualified System.IO              as IO
import           UnliftIO.Exception

-- * Effect

-- | An effect for reading and writing files.
data Filesystem :: Effect where
  ReadFile :: FilePath -> Filesystem m String
  WriteFile :: FilePath -> String -> Filesystem m ()

-- * Operations

makeEffect ''Filesystem

-- * Interpretations

-- | File system error.
newtype FsError = FsError String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Run the 'Filesystem' effect with actual file IO.
runFilesystemIO :: '[IOE, Error FsError] :>> es => Eff (Filesystem : es) a -> Eff es a
runFilesystemIO = interpret \case
  ReadFile path           -> adapt $ IO.readFile path
  WriteFile path contents -> adapt $ IO.writeFile path contents
  where
    adapt m = liftIO m `catch` \(e :: IOException) -> throwError $ FsError $ show e

-- | Run the 'Filesystem' effect with a faked filesystem.
runFilesystemPure :: Error FsError :> es => Map FilePath String -> Eff (Filesystem : es) a -> Eff es a
runFilesystemPure fs = fmap fst . runState fs . reinterpret \case
  ReadFile path -> maybeM (throwError $ FsError $ "File not found: " ++ show path) pure $ gets (M.lookup path)
  WriteFile path contents -> modify $ M.insert path contents

f :: Either FsError (Either FsError String)
f = runPure $ runError @FsError $ runFilesystemPure M.empty $ runError @FsError $ Filesystem.readFile "nonexistent"

-- >>> f
-- Left (FsError "File not found: \"nonexistent\"")
