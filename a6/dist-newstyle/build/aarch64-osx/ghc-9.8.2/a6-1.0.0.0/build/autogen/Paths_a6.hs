{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_a6 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/juliebrinch/.cabal/bin"
libdir     = "/Users/juliebrinch/.cabal/lib/aarch64-osx-ghc-9.8.2/a6-1.0.0.0-inplace"
dynlibdir  = "/Users/juliebrinch/.cabal/lib/aarch64-osx-ghc-9.8.2"
datadir    = "/Users/juliebrinch/.cabal/share/aarch64-osx-ghc-9.8.2/a6-1.0.0.0"
libexecdir = "/Users/juliebrinch/.cabal/libexec/aarch64-osx-ghc-9.8.2/a6-1.0.0.0"
sysconfdir = "/Users/juliebrinch/.cabal/etc"

getBinDir     = catchIO (getEnv "a6_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "a6_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "a6_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "a6_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "a6_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "a6_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
