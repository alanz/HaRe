#!/bin/sh
# From http://hspec.github.io/options.html

# cabal test --show-details=direct --test-option=--format=progress
# cabal new-run HaRe:spec -- "--format=progress --failure-report .hspec-failures --rerun"
# cabal new-run HaRe:spec -- "--match=\"Load\""
# cabal new-run HaRe:spec -- "--match=\"Utils\""
# cabal new-run spec -- --match="loads a single file"
# cabal new-run spec -- --match="UtilsSpec"
# cabal new-run spec --  --match "/Utils/UtilsSpec/parseSourceFileGhc/retrieves a module from an existing module graph #2/"
# cabal new-run spec -- --match="TypeUtilsSpec"
# cabal new-run spec -- --match="MoveDefSpec" --failure-report .hspec-failures --rerun
# cabal new-run spec -- --match="Renaming" --failure-report .hspec-failures --rerun


# stack test --test-arguments=--format=progress

# stack test --test-arguments "--format=progress --failure-report .hspec-failures --rerun"
# stack test --test-arguments "--failure-report .hspec-failures --rerun"


# --failure-report .hspec-failures
# --rerun
# --rerun-all-on-success
# ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/HaRe-0.8.4.1/t/spec/build/spec/spec --match  "loads a series of files based on cabal1 2"
# ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/HaRe-0.8.4.1/t/spec/build/spec/spec --match  "loads a series of files based on cabal1 1"
./dist-newstyle/build/x86_64-linux/ghc-8.6.5/HaRe-0.8.4.1/t/spec/build/spec/spec --match  "loading a file"
# ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/HaRe-0.8.4.1/t/spec/build/spec/spec --match  "loads the same file more than once"
