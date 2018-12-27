module CaseSpec (main, spec) where

import           Test.Hspec

import Language.Haskell.Refact.Refactoring.Case

import TestUtils

import System.Directory

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "ifToCase" $ do
    it "converts an if expression to a case expression BSimple" $ do
      r <- ct $ ifToCase defaultTestSettings testOptions "Case/BSimple.hs" (4,7) (4,43)
      -- r <- ct $ ifToCase logTestSettings testOptions  "Case/BSimple.hs" (4,7) (4,43)
      r' <- ct $ mapM makeRelativeToCurrentDirectory r
      r' `shouldBe` ["Case/BSimple.hs"]
      diff <- ct $ compareFiles "./Case/BSimple.refactored.hs"
                                "./Case/BSimpleExpected.hs"
      diff `shouldBe` []

    -- ---------------------------------

    it "converts an if expression to a case expression Foo" $ do
      r <- ct $ ifToCase defaultTestSettings testOptions "Case/Foo.hs" (4,1) (9,1)
      -- r <- ct $ ifToCase logTestSettings testOptions  "Case/Foo.hs" (4,1) (9,1)
      r' <- ct $ mapM makeRelativeToCurrentDirectory r
      r' `shouldBe` ["Case/Foo.hs"]
      diff <- ct $ compareFiles "./Case/Foo.refactored.hs"
                                "./Case/Foo.hs.expected"
      diff `shouldBe` []

    -- ---------------------------------

    it "converts an if expression to a case expression B" $ do
      r <- ct $ ifToCase defaultTestSettings testOptions "Case/B.hs" (4,7) (4,43)
      -- r <- ct $ ifToCase logTestSettings testOptions  "Case/B.hs" (4,7) (4,43)
      r' <- ct $ mapM makeRelativeToCurrentDirectory r
      r' `shouldBe` ["Case/B.hs"]
      diff <- ct $ compareFiles "./Case/B.refactored.hs"
                                "./Case/B.hs.expected"
      diff `shouldBe` []

    -- ---------------------------------

    it "converts an if expression with comments to a case expression 1 C" $ do

      r <- ct $ ifToCase defaultTestSettings testOptions "Case/C.hs" (5,7) (10,1)
      -- ct $ ifToCase logTestSettings testOptions "Case/C.hs" (5,7) (10,1)
      r' <- ct $ mapM makeRelativeToCurrentDirectory r
      r' `shouldBe` ["Case/C.hs"]
      diff <- ct $ compareFiles "./Case/C.refactored.hs"
                                "./Case/C.hs.expected"
      diff `shouldBe` []

    -- ---------------------------------

    it "converts an if expression with comments to a case expression 2 D" $ do
      r <- ct $ ifToCase defaultTestSettings testOptions "Case/D.hs" (5,7) (12,1)
      -- ct $ ifToCase logTestSettings testOptions "Case/D.hs" (5,7) (12,1)
      r' <- ct $ mapM makeRelativeToCurrentDirectory r
      r' `shouldBe` ["Case/D.hs"]
      diff <- ct $ compareFiles "./Case/D.refactored.hs"
                                "./Case/D.hs.expected"
      diff `shouldBe` []

    -- ---------------------------------

    it "converts in complex sub level expression 2 E" $ do
      r <- ct $ ifToCase defaultTestSettings testOptions "Case/E.hs" (7,8) (13,20)
      -- ct $ ifToCase logTestSettings testOptions "Case/E.hs" (7,8) (13,20)
      r' <- ct $ mapM makeRelativeToCurrentDirectory r
      r' `shouldBe` ["Case/E.hs"]
      diff <- ct $ compareFiles "./Case/E.refactored.hs"
                                "./Case/E.hs.expected"
      diff `shouldBe` []

    -- ---------------------------------

    it "converts in complex sub level expression F" $ do
      r <- ct $ ifToCase defaultTestSettings testOptions "Case/F.hs" (4,7) (8,20)
      -- ct $ ifToCase logTestSettings testOptions "Case/F.hs" (4,7) (8,20)
      r' <- ct $ mapM makeRelativeToCurrentDirectory r
      r' `shouldBe` ["Case/F.hs"]
      diff <- ct $ compareFiles "./Case/F.refactored.hs"
                                "./Case/F.hs.expected"
      diff `shouldBe` []

   -- ---------------------------------

    it "complains if an if-then-else is not selected" $ do
      res <- catchException(ct $ ifToCase defaultTestSettings testOptions "Case/C.hs" (4,7) (9,1))
      -- ifToCase logTestSettings testOptions "./Case/C.hs" (4,7) (9,1)
      (show res) `shouldBe` "Just \"You haven't selected an if-then-else  expression!\""

-- ---------------------------------------------------------------------
-- Helper functions
