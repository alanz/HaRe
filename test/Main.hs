{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import TestUtils
import qualified Turtle as Tu
import qualified Control.Foldl as Fold

import Test.Hspec.Runner
import qualified Spec

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  cleanupStackDirs
  setupStackFiles
  hspec Spec.spec

-- ---------------------------------------------------------------------

setupStackFiles :: IO ()
setupStackFiles =
  forM_ stackFiles $ \f ->
    writeFile f stackFileContents

-- ---------------------------------------------------------------------

stackFiles :: [FilePath]
stackFiles =
  [  "./test/testdata/stack.yaml"
   , "./test/testdata/cabal/cabal3/stack.yaml"
   , "./test/testdata/cabal/foo/stack.yaml"
   , "./test/testdata/cabal/cabal4/stack.yaml"
   , "./test/testdata/cabal/cabal1/stack.yaml"
   , "./test/testdata/cabal/cabal2/stack.yaml"
  ]


-- |Choose a resolver based on the current compiler, otherwise HaRe/ghc-mod will
-- not be able to load the files
resolver :: String
resolver =
#if __GLASGOW_HASKELL__ > 710
  "resolver: nightly-2016-08-25"
#else
  "resolver: lts-6.13"
#endif

-- ---------------------------------------------------------------------
{-

resolver: lts-6.13
# resolver: nightly-2016-08-25
packages:
- '.'
extra-deps: []
-}
stackFileContents :: String
stackFileContents = unlines
  [ "# WARNING: THIS FILE IS AUTOGENERATED IN test/Main.hs. IT WILL BE OVERWRITTEN ON EVERY TEST RUN"
  , resolver
  , "packages:"
  , "- '.'"
  , "extra-deps: []"
  ]

-- ---------------------------------------------------------------------

cleanupStackDirs :: IO ()
cleanupStackDirs = do
  dirs <- getStackDirs
  forM_ dirs  $ \dir -> Tu.rmtree dir

getStackDirs :: IO [Tu.FilePath]
getStackDirs = do
  let
    dirs = Tu.find (Tu.ends "/.stack-work") "./test"
  Tu.fold dirs Fold.list

listStackDirs :: IO ()
listStackDirs = Tu.sh $ do
  dirs <- Tu.find (Tu.ends "/.stack-work") "./test"
  Tu.echo $ "found:" Tu.<> (Tu.repr dirs)

