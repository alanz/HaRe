module MoveDefSpec (main, spec) where

import           Test.Hspec

import Language.Haskell.Refact.Refactoring.MoveDef
import System.Directory
import System.FilePath

import TestUtils

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do

 describe "MoveDefSpec" $ do

  -- -------------------------------------------------------------------

  describe "liftToTopLevel" $ do
    it "cannot lift a top level declaration" $ do
     -- res <- catchException (ct $ liftToTopLevel logTestSettings testOptions "./MoveDef/Md1.hs" (4,1))
     res <- catchException (ct $ liftToTopLevel defaultTestSettings testOptions "./MoveDef/Md1.hs" (4,1))
     (show res) `shouldBe` "Just \"\\nThe identifier is not a local function/pattern name!\""

    -- ---------------------------------

    it "checks for name clashes" $ do
     -- res <- catchException (doLiftToTopLevel ["./test/testdata/MoveDef/Md1.hs","17","5"])
     res <- catchException (ct $ liftToTopLevel defaultTestSettings testOptions "./MoveDef/Md1.hs" (17,5))
     (show res) `shouldBe` "Just \"The identifier(s): (ff, MoveDef/Md1.hs:17:5) will cause name clash/capture or ambiguity occurrence problem after lifting, please do renaming first!\""

    {-
    it "checks for invalid new name" $ do
     res <- catchException (doDuplicateDef ["./test/testdata/DupDef/Dd1.hs","$c","5","1"])
     (show res) `shouldBe` "Just \"Invalid new function name:$c!\""

    it "notifies if no definition selected" $ do
     res <- catchException (doDuplicateDef ["./test/testdata/DupDef/Dd1.hs","ccc","14","13"])
     (show res) `shouldBe` "Just \"The selected identifier is not a function/simple pattern name, or is not defined in this module \""
    -}

    -- ---------------------------------

    it "lifts a definition to the top level" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./MoveDef/Md1.hs" (24,5)
     -- r <- ct $ liftToTopLevel logTestSettings testOptions  "./test/testdata/MoveDef/Md1.hs" (24,5)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["MoveDef/Md1.hs"]
     diff <- ct $ compareFiles "./MoveDef/Md1.hs.expected"
                               "./MoveDef/Md1.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "liftToTopLevel D1 C1 A1 8 6" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/D1.hs" (8,6)
     -- r <- ct $ liftToTopLevel logTestSettings testOptions "./LiftToToplevel/D1.hs" (8,6)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/D1.hs"
                   ,"LiftToToplevel/C1.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/D1.hs.expected"
                               "./LiftToToplevel/D1.refactored.hs"
     diff `shouldBe` []

     diff2 <- ct $ compareFiles "./LiftToToplevel/C1.hs.expected"
                                "./LiftToToplevel/C1.refactored.hs"
     diff2 `shouldBe` []

     a1Refactored <- ct $ doesFileExist "./LiftToToplevel/A1.refactored.hs"
     a1Refactored `shouldBe` False


    -- ---------------------------------

    it "liftToTopLevel D2 C2 A2 8 6" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/D2.hs" (8,6)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/D2.hs"
                   ,"LiftToToplevel/C2.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/D2.hs.expected"
                               "./LiftToToplevel/D2.refactored.hs"
     diff `shouldBe` []

     diff2 <- ct $ compareFiles "./LiftToToplevel/C2.hs.expected"
                                "./LiftToToplevel/C2.refactored.hs"
     diff2 `shouldBe` []

     a1Refactored <- ct $ doesFileExist "./LiftToToplevel/A2.refactored.hs"
     a1Refactored `shouldBe` False


    -- ---------------------------------

    it "liftToTopLevel D3 C3 A3 8 6" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/D3.hs" (8,6)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/D3.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/D3.hs.expected"
                               "./LiftToToplevel/D3.refactored.hs"
     diff `shouldBe` []

     c3Refactored <- ct $ doesFileExist "./LiftToToplevel/C3.refactored.hs"
     c3Refactored `shouldBe` False

     a3Refactored <- ct $ doesFileExist "./LiftToToplevel/A3.refactored.hs"
     a3Refactored `shouldBe` False


    -- ---------------------------------

    it "liftToTopLevel WhereIn1 12 18" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/WhereIn1.hs" (12,18)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions    Nothing "./LiftToToplevel/WhereIn1.hs" (12,18)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/WhereIn1.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/WhereIn1.hs.expected"
                               "./LiftToToplevel/WhereIn1.refactored.hs"
     diff `shouldBe` []


    -- ---------------------------------

    it "liftToTopLevel WhereIn6 13 29" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/WhereIn6.hs" (13,29)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/WhereIn6.hs" (13,29)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/WhereIn6.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/WhereIn6.hs.expected"
                               "./LiftToToplevel/WhereIn6.refactored.hs"
     diff `shouldBe` []


    -- ---------------------------------

    it "liftToTopLevel WhereIn7 12 14" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/WhereIn7.hs" (12,14)
     -- r <- ct $ liftToTopLevel logTestSettings testOptions "./LiftToToplevel/WhereIn7.hs" (12,14)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/WhereIn7.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/WhereIn7.hs.expected"
                               "./LiftToToplevel/WhereIn7.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "liftToTopLevel LetIn1 11 22" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/LetIn1.hs" (11,22)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/LetIn1.hs" (11,22)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/LetIn1.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/LetIn1.hs.expected"
                               "./LiftToToplevel/LetIn1.refactored.hs"
     diff `shouldBe` []


    -- ---------------------------------

    it "liftToTopLevel LetIn2 10 22" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/LetIn2.hs" (10,22)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/LetIn2.hs" (10,22)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/LetIn2.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/LetIn2.hs.expected"
                               "./LiftToToplevel/LetIn2.refactored.hs"
     diff `shouldBe` []


    -- ---------------------------------

    it "liftToTopLevel LetIn3 10 27" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/LetIn3.hs" (10,27)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/LetIn3.hs" (10,27)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/LetIn3.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/LetIn3.hs.expected"
                               "./LiftToToplevel/LetIn3.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

{-
   This is trying to test an invalid lift

    it "liftToTopLevel PatBindIn1 18 7" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/PatBindIn1.hs" (18,7)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/PatBindIn1.hs" (18,7)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/PatBindIn1.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/PatBindIn1.hs.expected"
                               "./LiftToToplevel/PatBindIn1.refactored.hs"
     diff `shouldBe` []
-}

    -- ---------------------------------

    it "liftToTopLevel PatBindIn2 17 7 fails" $ do
     res <- catchException (ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/PatBindIn2.hs" (17,7))
     -- ct $ liftToTopLevel logTestSettings testOptions "./LiftToToplevel/PatBindIn2.hs" (17,7)

     (show res) `shouldBe`  "Just \"Cannot lift a declaration assigning to a tuple pattern\""

    -- ---------------------------------

    it "liftToTopLevel PatBindIn3 11 15" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/PatBindIn3.hs" (11,15)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/PatBindIn3.hs" (11,15)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/PatBindIn3.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/PatBindIn3.hs.expected"
                               "./LiftToToplevel/PatBindIn3.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "liftToTopLevel PatBindIn4 12 30 fails" $ do
     res <- catchException (ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/PatBindIn4.hs" (12,30))
     -- ct $ liftToTopLevel logTestSettings testOptions "./LiftToToplevel/PatBindIn4.hs" (12,30)

     (show res) `shouldBe` "Just \"Cannot lift a declaration assigning to a tuple pattern\""


    -- ---------------------------------

    it "liftToTopLevel CaseIn1 10 28" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/CaseIn1.hs" (10,28)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/CaseIn1.hs" (10,28)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/CaseIn1.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/CaseIn1.hs.expected"
                               "./LiftToToplevel/CaseIn1.refactored.hs"
     diff `shouldBe` []


    -- ---------------------------------

    it "liftToTopLevel WhereIn2 11 18 fails" $ do
     -- res <- catchException (doLiftToTopLevel ["./LiftToToplevel/WhereIn2.hs","11","18"])
     res <- catchException (ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/WhereIn2.hs" (11,18))
     -- liftToTopLevel logTestSettings  testOptions    Nothing "./LiftToToplevel/WhereIn2.hs" (11,18)

     (show res) `shouldBe` "Just \"The identifier(s): (sq, LiftToToplevel/WhereIn2.hs:11:18) will cause name clash/capture or ambiguity occurrence problem after lifting, please do renaming first!\""

    -- ---------------------------------

    it "liftToTopLevel Collapse1 8 6" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Collapse1.hs" (8,6)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/Collapse1.hs" (8,6)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/Collapse1.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/Collapse1.expected.hs"
                               "./LiftToToplevel/Collapse1.refactored.hs"
     diff `shouldBe` []


{- original tests
positive=[(["D1.hs","C1.hs","A1.hs"],["8","6"]),
          (["D2.hs","C2.hs","A2.hs"],["8","6"]),
          (["D3.hs","C3.hs","A3.hs"],["8","6"]),
          (["WhereIn1.hs"],["12","18"]),
          (["WhereIn6.hs"],["13","29"]),
          (["WhereIn7.hs"],["12","14"]),
          (["LetIn1.hs"],["11","22"]),
          (["LetIn2.hs"],["10","22"]),
          (["LetIn3.hs"],["10","27"]),
          (["PatBindIn1.hs"],["18","7"]),
          (["PatBindIn3.hs"],["11","15"]),
          (["CaseIn1.hs"],["10","28"])],
negative=[(["PatBindIn2.hs"],["17","7"]),
          (["WhereIn2.hs"],["11","18"])
         ]

-}

    -- ---------------------------------

    it "liftToTopLevel Zmapq" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Zmapq.hs" (6,3)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/Zmapq.hs" (6,3)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/Zmapq.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/Zmapq.expected.hs"
                               "./LiftToToplevel/Zmapq.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "liftToTopLevel LiftInLambda 10 5" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/LiftInLambda.hs" (10,5)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/LiftInLambda.hs" (10,5)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/LiftInLambda.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/LiftInLambda.expected.hs"
                               "./LiftToToplevel/LiftInLambda.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "liftToTopLevel NoWhere" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/NoWhere.hs" (14,12)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/NoWhere.hs" (14,12)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/NoWhere.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/NoWhere.expected.hs"
                               "./LiftToToplevel/NoWhere.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "liftToTopLevel Signature1" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Signature.hs" (9,5)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/Signature.hs" (9,5)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/Signature.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/Signature.expected.hs"
                               "./LiftToToplevel/Signature.refactored.hs"
     diff `shouldBe` []


    -- ---------------------------------

    it "liftToTopLevel Signature2" $ do
    -- should throw exception for forall in signature
     res <- catchException (ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Signature2.hs" (16,5))
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/Signature2.hs" (16,5)
     (show res) `shouldBe` "Just \"\\nNew type signature may fail type checking: :: (forall t. Num t => t -> t -> t) -> Int -> \\n\""

     {-
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Signature2.hs" (16,5)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/Signature2.hs" (16,5)
     (show r) `shouldBe` "[\"./LiftToToplevel/Signature2.hs"]
     diff <- compareFiles "./LiftToToplevel/Signature2.expected.hs"
                          "./LiftToToplevel/Signature2.refactored.hs"
     diff `shouldBe` []
     -}


    -- ---------------------------------

    it "liftToTopLevel Signature2r" $ do
    -- should throw exception for forall in signature
     r <- catchException (ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Signature2r.hs" (12,5))
     -- r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Signature2r.hs" (12,5)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/Signature2r.hs" (12,5)
     (show r) `shouldBe` "Just \"\\nNew type signature may fail type checking: :: (forall t. Num t => t -> t -> t) -> Int -> \\n\""
     {-
     (show r) `shouldBe` "[\"./LiftToToplevel/Signature2r.hs"]
     diff <- compareFiles "./LiftToToplevel/Signature2r.expected.hs"
                          "./LiftToToplevel/Signature2r.refactored.hs"
     diff `shouldBe` []
     -}

    -- ---------------------------------

    it "liftToTopLevel Signature3" $ do
     r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Signature3.hs" (9,5)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/Signature3.hs" (9,5)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftToToplevel/Signature3.hs"]
     diff <- ct $ compareFiles "./LiftToToplevel/Signature3.expected.hs"
                               "./LiftToToplevel/Signature3.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "liftToTopLevel Signature4" $ do
    -- should throw exception for forall in signature
     r <- catchException $ ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Signature4.hs" (9,5)
     -- r <- ct $ liftToTopLevel defaultTestSettings testOptions "./LiftToToplevel/Signature4.hs" (9,5)
     -- r <- ct $ liftToTopLevel logTestSettings  testOptions "./LiftToToplevel/Signature4.hs" (9,5)
     (show r) `shouldBe` "Just \"\\nNew type signature may fail type checking: :: (forall t. (Integral t, Num t) => t -> t -> Int) -> t -> \\n\""
     {-
     (show r) `shouldBe` "[\"./LiftToToplevel/Signature4.hs"]
     diff <- compareFiles "./LiftToToplevel/Signature4.expected.hs"
                          "./LiftToToplevel/Signature4.refactored.hs"
     diff `shouldBe` []
     -}

  -- -------------------------------------------------------------------

  describe "LiftOneLevel" $ do

    it "liftOneLevel.liftToMod D1 C1 A1 8 6" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/D1.hs" (8,6)
     -- r <- ct $ liftOneLevel logTestSettings testOptions "./LiftOneLevel/D1.hs" (8,6)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/D1.hs"
                   ,"LiftOneLevel/C1.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/D1.hs.expected"
                               "./LiftOneLevel/D1.refactored.hs"
     diff `shouldBe` []

     diff2 <- ct $ compareFiles "./LiftOneLevel/C1.hs.expected"
                                "./LiftOneLevel/C1.refactored.hs"
     diff2 `shouldBe` []

     a1Refactored <- ct $ doesFileExist "./LiftOneLevel/A1.refactored.hs"
     a1Refactored `shouldBe` False

    -- ---------------------------------

    it "LiftOneLevel.liftToMod D2 C2 A2 8 6" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/D2.hs" (8,6)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/D2.hs"
                   ,"LiftOneLevel/C2.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/D2.hs.expected"
                               "./LiftOneLevel/D2.refactored.hs"
     diff `shouldBe` []

     diff2 <- ct $ compareFiles "./LiftOneLevel/C2.hs.expected"
                                "./LiftOneLevel/C2.refactored.hs"
     diff2 `shouldBe` []

     a2Refactored <- ct $ doesFileExist "./LiftOneLevel/A2.refactored.hs"
     a2Refactored `shouldBe` False

    -- ---------------------------------

    it "LiftOneLevel.liftToMod D3 C3 A3 8 6" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/D3.hs" (8,6)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/D3.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/D3.hs.expected"
                               "./LiftOneLevel/D3.refactored.hs"
     diff `shouldBe` []

     c3Refactored <- doesFileExist "./LiftOneLevel/C3.refactored.hs"
     c3Refactored `shouldBe` False

     a3Refactored <- doesFileExist "./LiftOneLevel/A3.refactored.hs"
     a3Refactored `shouldBe` False

    -- ---------------------------------

    it "LiftOneLevel WhereIn1 12 18" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/WhereIn1.hs" (12,18)
     -- r <- ct $ liftOneLevel logTestSettings  testOptions "./LiftOneLevel/WhereIn1.hs" (12,18)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/WhereIn1.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/WhereIn1.hs.expected"
                               "./LiftOneLevel/WhereIn1.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "LiftOneLevel WhereIn6 13 29" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/WhereIn6.hs" (13,29)
     -- r <- ct $ liftOneLevel logTestSettings  testOptions "./LiftOneLevel/WhereIn6.hs" (13,29)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/WhereIn6.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/WhereIn6.hs.expected"
                               "./LiftOneLevel/WhereIn6.refactored.hs"
     diff `shouldBe` []


    -- ---------------------------------

    it "liftOneLevel WhereIn7 12 14" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/WhereIn7.hs" (12,14)
     -- r <- ct $ liftOneLevel logTestSettings  testOptions "./LiftOneLevel/WhereIn7.hs" (12,14)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/WhereIn7.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/WhereIn7.hs.expected"
                               "./LiftOneLevel/WhereIn7.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "LiftOneLevel WhereIn8 8 11" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/WhereIn8.hs" (8,11)
     -- r <- ct $ liftOneLevel logTestSettings  testOptions "./LiftOneLevel/WhereIn8.hs" (8,11)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/WhereIn8.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/WhereIn8.hs.expected"
                               "./LiftOneLevel/WhereIn8.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "LiftOneLevel LetIn1 11 22" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/LetIn1.hs" (11,22)
     -- r <- ct $ liftOneLevel logTestSettings  testOptions "./LiftOneLevel/LetIn1.hs" (11,22)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/LetIn1.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/LetIn1.hs.expected"
                               "./LiftOneLevel/LetIn1.refactored.hs"
     diff `shouldBe` []


    -- ---------------------------------

    it "LiftOneLevel LetIn2 11 22" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/LetIn2.hs" (11,22)
     -- r <- ct $ liftOneLevel logTestSettings  testOptions "./LiftOneLevel/LetIn2.hs" (11,22)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/LetIn2.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/LetIn2.hs.expected"
                               "./LiftOneLevel/LetIn2.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "LiftOneLevel LetIn3 10 27" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/LetIn3.hs" (10,27)
     -- r <- ct $ liftOneLevel logTestSettings  testOptions "./LiftOneLevel/LetIn3.hs" (10,27)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/LetIn3.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/LetIn3.hs.expected"
                               "./LiftOneLevel/LetIn3.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "LiftOneLevel PatBindIn3 11 15" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/PatBindIn3.hs" (11,15)
     -- r <- ct $ liftOneLevel logTestSettings  testOptions "./LiftOneLevel/PatBindIn3.hs" (11,15)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/PatBindIn3.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/PatBindIn3.hs.expected"
                               "./LiftOneLevel/PatBindIn3.refactored.hs"
     diff `shouldBe` []

    -- ---------------------------------

    it "liftOneLevel CaseIn1 10 28" $ do
     r <- ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/CaseIn1.hs" (10,28)
     -- r <- ct $ liftOneLevel logTestSettings  testOptions "./LiftOneLevel/CaseIn1.hs" (10,28)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["LiftOneLevel/CaseIn1.hs"]
     diff <- ct $ compareFiles "./LiftOneLevel/CaseIn1.hs.expected"
                               "./LiftOneLevel/CaseIn1.refactored.hs"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "fails PatBindIn2 17 7" $ do
     {-
     res <- catchException (liftOneLevel defaultTestSettings testOptions Nothing "./LiftOneLevel/PatBindIn2.hs" (17,7))
     -- liftOneLevel logTestSettings testOptions Nothing "./LiftOneLevel/PatBindIn2.hs" (17,7)
     (show res) `shouldBe` "Just \"Lifting this definition failed.  This might be because that the definition to be lifted is defined in a class/instance declaration.\""
     -}
     pending -- Not clear that this was covered in the original, will
             -- come back to it

    -- -----------------------------------------------------------------

    it "fails WhereIn2 8 18" $ do
     res <- catchException (ct $ liftOneLevel defaultTestSettings testOptions "./LiftOneLevel/WhereIn2.hs" (8,18))
     -- liftOneLevel logTestSettings testOptions "./LiftOneLevel/WhereIn2.hs" (8,18)
     (show res) `shouldBe` "Just \"The identifier(s): (sq, LiftOneLevel/WhereIn2.hs:8:18) will cause name clash/capture or ambiguity occurrence problem after lifting, please do renaming first!\""


-- TODO: check that other declarations in a list that make use of the
-- one being lifted also have params changed.
{- original tests
TestCases{refactorCmd="liftOneLevel",
positive=[(["D1.hs","C1.hs","A1.hs"],["8","6"]),
          (["D2.hs","C2.hs","A2.hs"],["8","6"]),
          (["D3.hs","C3.hs","A3.hs"],["8","6"]),
          (["WhereIn1.hs"],["12","18"]),
          (["WhereIn6.hs"],["15","29"]),
          (["WhereIn7.hs"],["12","14"]),
          (["WhereIn8.hs"],["8","11"]),
          (["LetIn1.hs"],["11","22"]),
          (["LetIn2.hs"],["10","22"]),
          (["LetIn3.hs"],["10","27"]),
          (["PatBindIn3.hs"],["11","15"]),
          (["CaseIn1.hs"],["10","28"])],
negative=[(["PatBindIn2.hs"],["17","7"]),
          (["WhereIn2.hs"],["8","18"])]
}


-}

  -- -------------------------------------------------------------------

  describe "demote" $ do

    it "notifies if no definition selected" $ do
     -- res <- catchException (doDemote ["./MoveDef/Md1.hs","14","13"])
     res <- catchException (ct $ demote defaultTestSettings testOptions "./MoveDef/Md1.hs" (14,13))
     (show res) `shouldBe` "Just \"\\nInvalid cursor position!\""

    it "will not demote if nowhere to go" $ do
     res <- catchException (ct $ demote defaultTestSettings testOptions "./MoveDef/Md1.hs" (8,1))
     -- res <- ct $ demote logTestSettings testOptions "./MoveDef/Md1.hs" (8,1)
     (show res) `shouldBe` "Just \"\\n Nowhere to demote this function!\\n\""

    -- -----------------------------------------------------------------

    it "demotes a definition from the top level 1" $ do
     r <- ct $ demote defaultTestSettings testOptions "./MoveDef/Demote.hs" (7,1)
     -- r <- ct $ demote logTestSettings testOptions "./MoveDef/Demote.hs" (7,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["MoveDef/Demote.hs"]
     diff <- ct $ compareFiles "./MoveDef/Demote.refactored.hs"
                               "./MoveDef/Demote.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes a definition from the top level D1" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/D1.hs" (9,1)
     -- r <- ct $ demote logTestSettings testOptions "./Demote/D1.hs" (9,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/D1.hs"]
     diff <- ct $ compareFiles "./Demote/D1.refactored.hs"
                               "./Demote/D1.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes WhereIn1 12 1" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/WhereIn1.hs" (12,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/WhereIn1.hs"]
     diff <- ct $ compareFiles "./Demote/WhereIn1.refactored.hs"
                               "./Demote/WhereIn1.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes WhereIn3 14 1" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/WhereIn3.hs" (14,1)
     -- r <- ct $ demote logTestSettings testOptions "./Demote/WhereIn3.hs" (14,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/WhereIn3.hs"]
     diff <- ct $ compareFiles "./Demote/WhereIn3.refactored.hs"
                               "./Demote/WhereIn3.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes WhereIn4 14 1" $ do
     -- r <- doDemote ["./Demote/WhereIn4.hs","14","1"]
     r <- ct $ demote defaultTestSettings testOptions "./Demote/WhereIn4.hs" (14,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/WhereIn4.hs"]
     diff <- ct $ compareFiles "./Demote/WhereIn4.refactored.hs"
                               "./Demote/WhereIn4.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes WhereIn5 14 1" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/WhereIn5.hs" (14,1)
     -- r <- ct $ demote logTestSettings testOptions "./Demote/WhereIn5.hs" (14,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/WhereIn5.hs"]
     diff <- ct $ compareFiles "./Demote/WhereIn5.refactored.hs"
                               "./Demote/WhereIn5.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes WhereIn6 13 1" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/WhereIn6.hs" (13,1)
     -- r <- ct $ demote logTestSettings testOptions "./Demote/WhereIn6.hs" (13,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/WhereIn6.hs"]
     diff <- ct $ compareFiles "./Demote/WhereIn6.refactored.hs"
                               "./Demote/WhereIn6.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes WhereIn7 13 1" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/WhereIn7.hs" (13,1)
     -- r <- ct $ demote logTestSettings testOptions "./Demote/WhereIn7.hs" (13,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/WhereIn7.hs"]
     diff <- ct $ compareFiles "./Demote/WhereIn7.refactored.hs"
                               "./Demote/WhereIn7.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes CaseIn1 16 1" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/CaseIn1.hs" (16,1)
     -- r <- ct $ demote logTestSettings testOptions "./Demote/CaseIn1.hs" (16,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/CaseIn1.hs"]
     diff <- ct $ compareFiles "./Demote/CaseIn1.refactored.hs"
                               "./Demote/CaseIn1.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes LetIn1 12 22" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/LetIn1.hs" (12,22)
     -- r <- ct $ demote logTestSettings testOptions "./Demote/LetIn1.hs" (12,22)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/LetIn1.hs"]
     diff <- ct $ compareFiles "./Demote/LetIn1.refactored.hs"
                               "./Demote/LetIn1.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes PatBindIn1 19 1" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/PatBindIn1.hs" (19,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/PatBindIn1.hs"]
     diff <- ct $ compareFiles "./Demote/PatBindIn1.refactored.hs"
                               "./Demote/PatBindIn1.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "demotes D2a 5 1 when not imported by other module" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/D2a.hs" (5,1)
     -- r <- ct $ demote logTestSettings testOptions "./Demote/D2a.hs" (5,1)
     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/D2a.hs"]
     diff <- ct $ compareFiles "./Demote/D2a.refactored.hs"
                               "./Demote/D2a.hs.expected"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "fails WhereIn2 14 1" $ do
     -- res <- catchException (doDemote ["./Demote/WhereIn2.hs","14","1"])
     res <- catchException (ct $ demote defaultTestSettings testOptions "./Demote/WhereIn2.hs" (14,1))
     -- demote (Just logSettings) testOptions Nothing "./Demote/WhereIn2.hs" (14,1)
     (show res) `shouldBe` "Just \"\\n Nowhere to demote this function!\\n\""

    -- -----------------------------------------------------------------

    it "fails LetIn2 11 22" $ do
     -- res <- catchException (doDemote ["./Demote/LetIn2.hs","11","22"])
     res <- catchException (ct $ demote defaultTestSettings testOptions "./Demote/LetIn2.hs" (11,22))
     (show res) `shouldBe` "Just \"This function can not be demoted as it is used in current level!\\n\""

    -- -----------------------------------------------------------------

    it "fails PatBindIn4 18 1" $ do
     -- res <- catchException (doDemote ["./Demote/PatBindIn4.hs","18","1"])
     res <- catchException (ct $ demote defaultTestSettings testOptions "./Demote/PatBindIn4.hs" (18,1))
     -- (show res) `shouldBe` "Just \"\\n Nowhere to demote this function!\\n\""
     (show res) `shouldBe` "Just \"\\nThis function/pattern binding is used by more than one friend bindings\\n\""

    -- -----------------------------------------------------------------

    it "fails WhereIn8 16 1" $ do
     -- res <- catchException (doDemote ["./Demote/WhereIn8.hs","16","1"])
     res <- catchException (ct $ demote defaultTestSettings testOptions "./Demote/WhereIn8.hs" (16,1))
     (show res) `shouldBe` "Just \"\\n Nowhere to demote this function!\\n\""

    -- -----------------------------------------------------------------

    it "fails D2 5 1" $ do
     res <- catchException (ct $ demote defaultTestSettings testOptions "./Demote/D2.hs" (5,1))
     -- res <- catchException (ct $ demote logTestSettings testOptions "./Demote/D2.hs" (5,1))
     (show res) `shouldBe` "Just \"This definition can not be demoted, as it is used in the client module 'Main'!\""

    -- -----------------------------------------------------------------

    it "fails for re-export in client module"  $ do
      pending

    -- -----------------------------------------------------------------

    it "fails D3 5 1" $ do
     -- res <- catchException (doDemote ["./Demote/D3.hs","5","1"])
     res <- catchException (ct $ demote defaultTestSettings testOptions "./Demote/D3.hs" (5,1))
     (show res) `shouldBe` "Just \"This definition can not be demoted, as it is explicitly exported by the current module!\""



{- Original test cases. These files are now in testdata/Demote

TestCases{refactorCmd="demote",
positive=[(["D1.hs","C1.hs","A1.hs"],["9","1"]), x
          (["WhereIn1.hs"],["12","1"]), x
          (["WhereIn3.hs"],["14","1"]), x
          (["WhereIn4.hs"],["14","1"]), x
          (["WhereIn5.hs"],["14","1"]), x
          (["WhereIn6.hs"],["13","1"]), x
          (["WhereIn7.hs"],["13","1"]), x
          (["CaseIn1.hs"],["16","1"]), x
          (["LetIn1.hs"],["12","22"]), x
          (["PatBindIn1.hs"],["19","1"])], x
negative=[(["WhereIn2.hs"],["14","1"]), x
          (["LetIn2.hs"],["11","22"]), x
          (["PatBindIn4.hs"],["18","1"]), x
          (["WhereIn8.hs"],["16","1"]), x
          (["D2.hs","C2.hs","A2.hs"],["5","1"]), x
          (["D3.hs"],["5","1"])] x
}
-}

    -- -----------------------------------------------------------------

    it "fails MultiLeg.hs" $ do
     res <- catchException (ct $ demote defaultTestSettings testOptions "./Demote/MultiLeg.hs" (14,1))
     -- demote logTestSettings testOptions "./Demote/MultiLeg.hs" (14,1)
     (show res) `shouldBe` "Just \"\\nThis function/pattern binding is used by more than one friend bindings\\n\""


    -- -----------------------------------------------------------------

    it "passes MultiLeg2.hs" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/MultiLeg2.hs" (14,1)
     -- demote logTestSettings testOptions "./Demote/MultiLeg2.hs" (14,1)

     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/MultiLeg2.hs"]
     diff <- ct $ compareFiles "./Demote/MultiLeg2.refactored.hs"
                               "./Demote/MultiLeg2.hs.expected"
     diff `shouldBe` []


    -- -----------------------------------------------------------------

    it "passes UsedAtLevel.hs" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/UsedAtLevel.hs" (19,12)
     -- demote logTestSettings testOptions "./Demote/UsedAtLevel.hs" (19,12)

     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/UsedAtLevel.hs"]
     diff <- ct $ compareFiles "./Demote/UsedAtLevel.refactored.hs"
                               "./Demote/UsedAtLevel.expected.hs"
     diff `shouldBe` []

    -- -----------------------------------------------------------------

    it "passes UsedAtLevel.hs2" $ do
     r <- ct $ demote defaultTestSettings testOptions "./Demote/UsedAtLevel2.hs" (23,12)
     -- demote logTestSettings testOptions "./Demote/UsedAtLevel2.hs" (23,12)

     r' <- ct $ mapM makeRelativeToCurrentDirectory r
     r' `shouldBe` ["Demote/UsedAtLevel2.hs"]
     diff <- ct $ compareFiles "./Demote/UsedAtLevel2.refactored.hs"
                               "./Demote/UsedAtLevel2.expected.hs"
     diff `shouldBe` []


-- ---------------------------------------------------------------------
-- Helper functions
