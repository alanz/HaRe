module Language.Haskell.Refact.Refactoring.Sugar (sugar, compSugar) where

import Language.Haskell.Refact.API
import qualified GhcModCore as GM (Options(..))
import System.Directory (canonicalizePath)
-- import qualified Data.Generics as SYB
-- import qualified GHC as GHC

sugar :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]
sugar settings cradle fileName pos = do
  absFileName <- canonicalizePath fileName
  runRefacSession settings cradle (compSugar absFileName pos)

compSugar :: FilePath -> SimpPos -> RefactGhc [ApplyRefacResult]
compSugar fileName pos = do
  (refRes@((_fp,ismod),_), ()) <- applyRefac (doSugaring pos) (RSFile fileName)
  case ismod of
    RefacUnmodifed -> error "Desugaring failed"
    RefacModified -> return ()
  return [refRes]

doSugaring :: SimpPos -> RefactGhc ()
doSugaring pos = do
  parsed <- getRefactParsed

  return ()
