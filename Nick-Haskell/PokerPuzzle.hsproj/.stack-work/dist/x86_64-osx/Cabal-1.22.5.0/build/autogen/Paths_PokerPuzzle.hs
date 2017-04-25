module Paths_PokerPuzzle (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/nickager/Documents/programming/poker-puzzle/Nick-Haskell/PokerPuzzle.hsproj/.stack-work/install/x86_64-osx/lts-6.29/7.10.3/bin"
libdir     = "/Users/nickager/Documents/programming/poker-puzzle/Nick-Haskell/PokerPuzzle.hsproj/.stack-work/install/x86_64-osx/lts-6.29/7.10.3/lib/x86_64-osx-ghc-7.10.3/PokerPuzzle-1.0-5p8fo0f2n8Y4TnEwbHQZfY"
datadir    = "/Users/nickager/Documents/programming/poker-puzzle/Nick-Haskell/PokerPuzzle.hsproj/.stack-work/install/x86_64-osx/lts-6.29/7.10.3/share/x86_64-osx-ghc-7.10.3/PokerPuzzle-1.0"
libexecdir = "/Users/nickager/Documents/programming/poker-puzzle/Nick-Haskell/PokerPuzzle.hsproj/.stack-work/install/x86_64-osx/lts-6.29/7.10.3/libexec"
sysconfdir = "/Users/nickager/Documents/programming/poker-puzzle/Nick-Haskell/PokerPuzzle.hsproj/.stack-work/install/x86_64-osx/lts-6.29/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PokerPuzzle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PokerPuzzle_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PokerPuzzle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PokerPuzzle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PokerPuzzle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
