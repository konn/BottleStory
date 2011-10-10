module Paths_BottleStory (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/hiromi/Documents/Programming/haskell/yesod/BottleStory/cabal-dev//bin"
libdir     = "/Users/hiromi/Documents/Programming/haskell/yesod/BottleStory/cabal-dev//lib/BottleStory-0.0.0/ghc-7.0.3"
datadir    = "/Users/hiromi/Documents/Programming/haskell/yesod/BottleStory/cabal-dev//share/BottleStory-0.0.0"
libexecdir = "/Users/hiromi/Documents/Programming/haskell/yesod/BottleStory/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "BottleStory_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "BottleStory_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "BottleStory_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "BottleStory_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
