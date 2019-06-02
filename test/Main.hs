{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import TestUtils
import qualified Turtle as Tu
import qualified Control.Foldl as Fold
import System.Directory
import System.Process (readProcess)

import Test.Hspec.Runner
import qualified Spec

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  -- setLogger
  cleanupDirs (Tu.ends     "/stack.yaml")
  cleanupDirs (Tu.ends     "/.stack-work")
  cleanupDirs (Tu.ends     "/dist")
  cleanupDirs (Tu.ends     "/dist-newstyle")
  cleanupDirs (Tu.contains ".ghc.environ")
  -- if True
  if False
    then setupStackFiles
    else setupDistDirs
  hspec Spec.spec

-- ---------------------------------------------------------------------

setupStackFiles :: IO ()
setupStackFiles =
  forM_ stackFiles $ \f ->
    writeFile f stackFileContents

setupDistDirs :: IO ()
setupDistDirs =
  forM_ cabalDirs $ \d -> do
    withCurrentDirectory d $ do
      -- run "cabal" [ "install", "--dependencies-only" ]
      -- run "cabal" [ "configure" ]

      run "cabal" [ "new-configure" ]
      -- run "cabal" [ "new-build" ]

-- This is shamelessly copied from cabal-helper GhcSession test.
run :: String -> [String] -> IO ()
run x xs = do
  print $ x:xs
  o <- readProcess x xs ""
  putStrLn o
  return ()

-- ---------------------------------------------------------------------

cabalDirs :: [FilePath]
cabalDirs =
  [  "./test/testdata/"
   , "./test/testdata/cabal/cabal3/"
   , "./test/testdata/cabal/foo/"
   , "./test/testdata/cabal/cabal4/"
   , "./test/testdata/cabal/cabal1/"
   , "./test/testdata/cabal/cabal2/"
  ]

stackFiles :: [FilePath]
stackFiles = map (++"stack.yaml") cabalDirs

-- |Choose a resolver based on the current compiler, otherwise HaRe/ghc-mod will
-- not be able to load the files
resolver :: String
resolver =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,5,0)))
  "resolver: lts-13.21"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,4,0)))
  "resolver: lts-13.16"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,3,0)))
  "resolver: lts-13.9"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,1,0)))
  "resolver: nightly-2018-11-07"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,4,0)))
  "resolver: lts-12.17"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,3,0)))
  "resolver: nightly-2018-06-29"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)))
  "resolver: nightly-2018-04-24"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,1,0)))
  "resolver: nightly-2018-04-21" -- last one for GHC 8.4.1
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,2,2,0)))
  "resolver: lts-11.6"
#elif __GLASGOW_HASKELL__ >= 802
  "resolver: nightly-2017-09-02"
#elif __GLASGOW_HASKELL__ > 710
  "resolver: nightly-2017-05-23"
#else
  "resolver: lts-6.13"
#endif

-- ---------------------------------------------------------------------

stackFileContents :: String
stackFileContents = unlines
  [ "# WARNING: THIS FILE IS AUTOGENERATED IN test/Main.hs. IT WILL BE OVERWRITTEN ON EVERY TEST RUN"
  , resolver
  , "packages:"
  , "- '.'"
  , "extra-deps: "
  -- , "- conversion-1.2.1"
  -- , "- conversion-bytestring-1.0.1"
  -- , "- conversion-case-insensitive-1.0.0.0"
  -- , "- conversion-text-1.0.1"
  -- , "- syb-0.7"
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,2,0)))
    , "- conversion-1.2.1"
    , "- conversion-case-insensitive-1.0.0.0"
    , "- conversion-text-1.0.1"
    , "- conversion-bytestring-1.0.1"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,1,0)))
  -- , "- Cabal-2.2.0.0"
  -- , "- attoparsec-0.13.2.2"
  -- , "- base-prelude-1.2.0.1"
  -- , "- case-insensitive-1.2.0.10"
  -- , "- hashable-1.2.7.0"
  -- , "- scientific-0.3.5.2"
  -- , "- integer-logarithms-1.0.2.1"
  -- , "- primitive-0.6.3.0"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,2,2,0)))
    , "- conversion-1.2.1"
    , "- conversion-bytestring-1.0.1"
    , "- conversion-case-insensitive-1.0.0.0"
    , "- conversion-text-1.0.1"
    , "- syb-0.7"
#elif __GLASGOW_HASKELL__ >= 802
    , "- conversion-1.2.1"
    , "- conversion-bytestring-1.0.1"
    , "- conversion-case-insensitive-1.0.0.0"
    , "- conversion-text-1.0.1"
    , "- syb-0.7"
#elif __GLASGOW_HASKELL__ > 710
#endif
  ]

-- ---------------------------------------------------------------------

cleanupDirs :: Tu.Pattern t -> IO ()
cleanupDirs ending = do
  dirs <- getDirs ending
  forM_ dirs  $ \dir -> Tu.rmtree dir

getDirs :: Tu.Pattern t -> IO [Tu.FilePath]
getDirs ending = do
  let
    -- dirs = Tu.find (Tu.ends "/.stack-work") "./test"
    dirs = Tu.find ending "./test"
  Tu.fold dirs Fold.list

listStackDirs :: IO ()
listStackDirs = Tu.sh $ do
  dirs <- Tu.find (Tu.ends "/.stack-work") "./test"
  mapM Tu.echo $ Tu.textToLines $ "found:" Tu.<> (Tu.repr dirs)
