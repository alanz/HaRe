{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Language.Haskell.Refact.Refactoring.DupDef
  ( duplicateDef
  , compDuplicateDef ) where

import qualified Data.Generics as SYB

import qualified GHC
import qualified RdrName       as GHC

import Data.List
import Data.Maybe

import qualified GhcModCore   as GM (Options(..),mpPath)
import Language.Haskell.Refact.API

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Utils

import System.Directory

-- ---------------------------------------------------------------------
-- | This refactoring duplicates a definition (function binding or
-- simple pattern binding) at the same level with a new name provided by
-- the user. The new name should not cause name clash/capture.
duplicateDef :: RefactSettings -> GM.Options -> FilePath -> String -> SimpPos -> IO [FilePath]
duplicateDef settings opts fileName newName (row,col) = do
  absFileName <- normaliseFilePath fileName
  runRefacSession settings opts (compDuplicateDef absFileName newName (row,col))

compDuplicateDef :: FilePath -> String -> SimpPos -> RefactGhc [ApplyRefacResult]
compDuplicateDef fileName newName (row, col) = do
  if isVarId newName
    then
      do
        parseSourceFileGhc fileName
        renamed <- getRefactRenamed
        parsed  <- getRefactParsed
        nm <- getRefactNameMap
        targetModule <- getRefactTargetModule
        logm $ "DupDef.compDuplicateDef:got targetModule"

        let (Just (modName,_)) = getModuleName parsed
        let maybePn = locToRdrName (row,col) parsed
        -- let maybePn = locToNamePure nm (row, col) parsed
        case maybePn of
          Just lr@(GHC.L l _) ->
            do
              let pn = GHC.L l (rdrName2NamePure nm lr)
              logm $ "DupDef.compDuplicateDef:about to applyRefac for:pn=" ++ showAnnData mempty 0 pn
              (refactoredMod,(isDone,nn)) <- applyRefac (doDuplicating pn newName) RSAlreadyLoaded
              logm $ "DupDef.com:isDone=" ++ show isDone
              case isDone of
                DupDefFailed -> error "The selected identifier is not a function/simple pattern name, or is not defined in this module "
                DupDefLowerLevel -> return [refactoredMod]
                DupDefTopLevel -> do
                  if modIsExported modName renamed
                   then
                    do
                       logm $ "DupDef.compDuplicateDef:about to clientMods"
                       clients <- clientModsAndFiles targetModule
                       logm ("DupDef: clients=" ++ (showGhc clients)) -- ++AZ++ debug
                       refactoredClients <- mapM (refactorInClientMod (GHC.unLoc pn) modName nn)
                                                 clients
                       return $ refactoredMod:refactoredClients
                   else return [refactoredMod]
          Nothing -> error "Invalid cursor position!"
    else error $ "Invalid new function name:" ++ newName ++ "!"


data DupDefResult = DupDefFailed | DupDefTopLevel | DupDefLowerLevel
                  deriving (Eq,Show)

doDuplicating :: GHC.Located GHC.Name -> String
              -> RefactGhc (DupDefResult,GHC.Name)
doDuplicating pn newName = do
   logm $ "DupDef.compDuplicateDef:doDuplicating entered"
   inscopes <- getRefactInscopes
   logm $ "DupDef.compDuplicateDef:doDuplicating got inscopes"
   reallyDoDuplicating pn newName inscopes


reallyDoDuplicating :: GHC.Located GHC.Name -> String
              -> InScopes
              -> RefactGhc (DupDefResult,GHC.Name)
reallyDoDuplicating pn newName _inscopes = do
   clearRefactDone
   parsed <- getRefactParsed
   newNameGhc <- mkNewGhcName Nothing newName

   -- Check if it is a top level dup
   parsed2 <- dupInModule newNameGhc parsed
   d <- getRefactDone
   if d
     then do
       putRefactParsed parsed2 emptyAnns
       return (DupDefTopLevel,newNameGhc)
     else do
       parsed' <- SYB.everywhereM (
                                   SYB.mkM    (dupInMatch   newNameGhc)
                                   `SYB.extM` (dupInPat     newNameGhc)
                                   `SYB.extM` (dupInLet     newNameGhc)
                                   `SYB.extM` (dupInLetStmt newNameGhc)
                                  ) parsed2
       putRefactParsed parsed' emptyAnns
       done <- getRefactDone
       if done then return (DupDefLowerLevel,newNameGhc)
               else return (DupDefFailed,newNameGhc)

        where
        --1. The definition to be duplicated is at top level.
        dupInModule :: GHC.Name -> GHC.ParsedSource -> RefactGhc GHC.ParsedSource
        dupInModule newNameGhc p
          = do
              declsp <- liftT $ hsDecls p
              nm <- getRefactNameMap
              if not $ emptyList (findFunOrPatBind nm pn declsp)
                then doDuplicating' newNameGhc p pn
                else return p

        --2. The definition to be duplicated is a local declaration in a match
        dupInMatch newNameGhc (match::GHC.LMatch GhcPs (GHC.LHsExpr GhcPs))
          = do
              nm <- getRefactNameMap
              declsp <- liftT $ hsDecls match
              logm $ "dupInMatch:declsp=" ++ showGhc declsp
              if not $ emptyList (findFunOrPatBind nm pn declsp)
                then doDuplicating' newNameGhc match pn
                else return match

        --3. The definition to be duplicated is a local declaration in a pattern binding
#if __GLASGOW_HASKELL__ >= 806
        dupInPat newNameGhc (pat@(GHC.L _ (GHC.ValD _ (GHC.PatBind {}))) :: GHC.LHsDecl GhcPs)
#else
        dupInPat newNameGhc (pat@(GHC.L _ (GHC.ValD (GHC.PatBind {}))) :: GHC.LHsDecl GhcPs)
#endif
          = do
            logm $ "dupInPat hit"
            doDuplicating' newNameGhc pat pn
        dupInPat _ pat = return pat

        --4: The defintion to be duplicated is a local decl in a Let expression
        dupInLet newNameGhc (letExp@(GHC.L _ (GHC.HsLet {})):: GHC.LHsExpr GhcPs)
          = doDuplicating' newNameGhc letExp pn
        dupInLet _ letExp = return letExp

        --5. The definition to be duplicated is a local decl in a case alternative.
        -- Note: The local declarations in a case alternative are covered in #2 above.

        --6.The definition to be duplicated is a local decl in a Let statement.
        dupInLetStmt newNameGhc (letStmt@(GHC.L _ (GHC.LetStmt {})):: GHC.LStmt GhcPs (GHC.LHsExpr GhcPs))
          = doDuplicating' newNameGhc letStmt pn
        dupInLetStmt _ letStmt = return letStmt


        findFunOrPatBind nm (GHC.L _ n) ds
          = filter (\d->isFunBindP d || isSimplePatDecl d) $ definingDeclsRdrNames nm [n] ds True False


        doDuplicating' :: (SYB.Data t) => GHC.Name -> t -> GHC.Located GHC.Name -> RefactGhc t
        doDuplicating' newNameGhc t _ln = do
          logm $ "doDuplicating' entered"
          -- logm $ "doDuplicating' entered:t=" ++ showAnnData mempty 0 t
          -- declsp <- liftT $ hsDecls t
          declsp <- liftT $ hsDeclsGeneric t
          nm <- getRefactNameMap
          if not $ emptyList (findFunOrPatBind nm pn declsp)
            then doDuplicating'' newNameGhc t pn
            else return t


        doDuplicating'' :: (SYB.Data t) => GHC.Name -> t -> GHC.Located GHC.Name
                       -> RefactGhc t
        doDuplicating'' newNameGhc parentr ln = do
          -- logm $ "doDuplicating'':parentr=" ++ showAnnData mempty 0 parentr
          r <- hasDeclsSybTransform workerHsDecls workerBind parentr
          logm $ "doDuplicating'':done"
          return r
          where
              workerHsDecls :: forall t. HasDecls t => t -> RefactGhc t
              workerHsDecls t = do
                logm $ "workerHsDecls hit"
                ds <- liftT $ hsDecls t
                ds' <- doDuplicating3 newNameGhc parentr ln ds
                liftT $ replaceDecls t ds'

              workerBind :: (GHC.LHsBind GhcPs -> RefactGhc (GHC.LHsBind GhcPs))
              workerBind t@(GHC.L _ (GHC.PatBind{})) = do
                logm $ "workerBind hit"
                ds <- liftT $ hsDeclsPatBind t
                ds' <- doDuplicating3 newNameGhc t ln ds
                liftT $ replaceDeclsPatBind t ds'
              workerBind x = error $ "DupDef.doDuplicating'':workerBind got:" ++ showGhc x

        doDuplicating3 :: (SYB.Data t) => GHC.Name -> t -> GHC.Located GHC.Name
                       -> [GHC.LHsDecl GhcPs]
                       -> RefactGhc [GHC.LHsDecl GhcPs]
        doDuplicating3 newNameGhc parentr ln@(GHC.L _ n) declsp
           = do
                logm $ "doDuplicating'' entered:ln" ++ showGhc ln
                -- declsp <- liftT $ hsDecls parentr
                nm <- getRefactNameMap
                let
                    duplicatedDecls = definingDeclsRdrNames nm [n] declsp True False

                logm $ "doDuplicating'':duplicatedDecls=" ++ showGhc duplicatedDecls
                (f,d) <- hsFDNamesFromInsideRdr parentr
                    --f: names that might be shadowd by the new name,
                    --d: names that might clash with the new name

                logm $ "doDuplicating'':(f,d)=" ++ show (f,d)
                DN dv <- hsVisibleDsRdr nm n declsp --dv: names may shadow new name
                let vars        = nub (f `union` d `union` map showGhc dv)

                -- TODO: Where definition is of form tup@(h,t), test each element of it for clashes, or disallow
                nameAlreadyInScope <- isInScopeAndUnqualifiedGhc newName Nothing

                if elem newName vars || (nameAlreadyInScope && findNameInRdr nm n duplicatedDecls)
                   then error ("The new name'"++newName++"' will cause name clash/capture or ambiguity problem after "
                               ++ "duplicating, please select another name!")
                   else do
                           setRefactDone
                           newdecls <- duplicateDecl declsp n newNameGhc
                           -- parentr' <- liftT $ replaceDecls parentr newdecls
                           -- return parentr'
                           return newdecls


-- | Do refactoring in the client module. That is to hide the
-- identifer in the import declaration if it will cause any problem in
-- the client module.
refactorInClientMod :: GHC.Name -> GHC.ModuleName -> GHC.Name -> TargetModule
                    -> RefactGhc ApplyRefacResult
refactorInClientMod oldPN serverModName newPName targetModule
  = do
       logm ("refactorInClientMod: (oldPN,serverModName,newPName)=" ++ (showGhc (oldPN,serverModName,newPName)))
       getTargetGhc targetModule

       let fileName = GM.mpPath targetModule
       renamed <- getRefactRenamed
       parsed  <- getRefactParsed

       logm $ "refactorInClientMod:got newPName:" ++ showGhcQual newPName
       let modNames = willBeUnQualImportedBy serverModName renamed
       logm ("refactorInClientMod: (modNames)=" ++ (showGhc (modNames))) -- ++AZ++ debug

       mustHide <- needToBeHided newPName parsed
       logm ("refactorInClientMod: (mustHide)=" ++ (showGhc (mustHide))) -- ++AZ++ debug
       if isJust modNames && mustHide
        then do
                (refactoredMod,_) <- applyRefac (doDuplicatingClient serverModName [newPName]) (RSFile fileName)
                return refactoredMod
        else return ((fileName,RefacUnmodifed),(emptyAnns,parsed))
   where
     needToBeHided :: GHC.Name -> GHC.ParsedSource -> RefactGhc Bool
     needToBeHided name parsed = do
         -- logDataWithAnns "refactorInClientMod.needToBeHided:parsed" parsed
         -- logm $ "refactorInClientMod.needToBeHided:name=" ++ showGhcQual name
         nm <- getRefactNameMap
         logm $ "refactorInClientMod.needToBeHided:nm=" ++ showGhcQual nm
         let usedUnqual = usedWithoutQualR name parsed
         logm ("refactorInClientMod: (usedUnqual)=" ++ (showGhc (usedUnqual))) -- ++AZ++ debug
         return $ usedUnqual || causeNameClashInExports nm oldPN name serverModName parsed

doDuplicatingClient :: GHC.ModuleName -> [GHC.Name]
              -> RefactGhc ()
doDuplicatingClient serverModName newPNames = do
  logm $ "doDuplicatingClient:newPNames=" ++ showGhc newPNames
  parsed  <- getRefactParsed
  parsed' <- addHiding serverModName parsed (map GHC.nameRdrName newPNames)
  putRefactParsed parsed' emptyAnns
  return ()



--Check here:
-- | get the module name or alias name by which the duplicated
-- definition will be imported automatically.
willBeUnQualImportedBy :: GHC.ModuleName -> GHC.RenamedSource -> Maybe [GHC.ModuleName]
willBeUnQualImportedBy modName (_,imps,_,_)
   = let
         -- ms = filter (\(GHC.L _ (GHC.ImportDecl _ (GHC.L _ modName1) _ _ _ isQualified _ _ h))
         ms = filter (\(GHC.L _ (GHC.ImportDecl { GHC.ideclName = (GHC.L _ modName1)
                                                , GHC.ideclQualified = isQualified
                                                , GHC.ideclHiding = h}))
                    -> modName == modName1
                       && not isQualified
                              && (isNothing h  -- not hiding
                                  ||
                                   (isJust h && ((fst (gfromJust "willBeUnQualImportedBy" h))==True))
                                  ))
                      imps
         in if (emptyList ms) then Nothing
                      else Just $ nub $ map getModName ms

         where getModName (GHC.L _ (GHC.ImportDecl { GHC.ideclAs = as }))
#if __GLASGOW_HASKELL__ <= 800
                 = if isJust as then (fromJust as)
#else
                 = if isJust as then (GHC.unLoc $ fromJust as)
#endif
                                else modName
