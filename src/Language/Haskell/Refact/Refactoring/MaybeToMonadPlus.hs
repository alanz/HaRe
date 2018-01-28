{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Refact.Refactoring.MaybeToMonadPlus where

import Language.Haskell.Refact.API
import qualified GhcModCore as GM (Options(..))
import System.Directory
import qualified GHC as GHC
import Data.Generics as SYB
import GHC.SYB.Utils as SYB
import Data.Generics.Strafunski.StrategyLib.StrategyLib
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Types
import Control.Applicative
import qualified Data.Map as Map
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified Type as GHC
import FastString

maybeToMonadPlus :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> String -> Int -> IO [FilePath]
maybeToMonadPlus settings cradle fileName pos funNm argNum = do
  absFileName <- canonicalizePath fileName
  runRefacSession settings cradle (comp absFileName pos funNm argNum)

comp :: FilePath -> SimpPos -> String -> Int -> RefactGhc [ApplyRefacResult]
comp fileName (row,col) funNm argNum = do
  (refRes@((_fp,ismod), _),()) <- applyRefac (doMaybeToPlus fileName (row,col) funNm argNum) (RSFile fileName)
  case ismod of
    RefacUnmodifed -> error "Maybe to MonadPlus synonym failed"
    RefacModified -> return ()
  return [refRes]


doMaybeToPlus :: FilePath -> SimpPos -> String -> Int -> RefactGhc ()
doMaybeToPlus fileName pos funNm argNum = do
  parsed <- getRefactParsed
  -- Add test that position defines function with name `funNm`
  let mBind = getHsBind pos parsed
  case mBind of
    Nothing -> error "Function bind not found"
    Just funBind -> do
      canReplaceConstructors <- isOutputType argNum pos funBind
      case canReplaceConstructors of
        True -> do
          logm $ "Can replace constructors"
          replaceConstructors pos funNm argNum
        False -> do
          logm $ "Can't replace constructors"
          hasNtoN <- containsNothingToNothing funNm argNum pos funBind
          logm $ "Result of searching for nothing to nothing: " ++ (show hasNtoN)
          case hasNtoN of
            False -> return ()
            True  -> doRewriteAsBind pos funNm
          return ()

-- This checks if the argument to be refactored is the output type
-- If this is true then the refactoring will just consist of replacing all RHS
-- calls to the Maybe type with their MPlus equivalents
-- I need some way of checking if the type
isOutputType :: Int -> SimpPos -> GHC.HsBind GHC.RdrName -> RefactGhc Bool
isOutputType argNum pos funBind = do
  -- renamed <- getRefactRenamed
  parsed <- getRefactParsed
  (Just name) <- locToNameRdr pos parsed
  (Just ty) <- getTypeForName name
  logDataWithAnns "isOutputType:ty" ty
  let depth = typeDepth ty
  logm $ "isOutputType:depth=" ++ show depth
  return $ depth == argNum
    where typeDepth :: GHC.Type -> Int
          typeDepth ty = case (GHC.isFunTy ty) of
            True -> 1 + typeDepth (GHC.funResultTy ty)
            False -> 1

--This handles the case where only the output type of the function is being modified so calls to
--Nothing and Just can be replaced with mzero and return respectively in GRHSs
replaceConstructors :: SimpPos -> String -> Int -> RefactGhc ()
replaceConstructors pos funNm argNum = do
  parsed <- getRefactParsed
  let (Just bind) = getHsBind pos parsed
  newBind <- applyInGRHSs bind replaceNothingAndJust
  replaceBind pos newBind
  fixType' funNm argNum
    where applyInGRHSs :: (Data a) => UnlocParsedHsBind -> (a -> RefactGhc a) -> RefactGhc UnlocParsedHsBind
          applyInGRHSs parsed fun = applyTP (stop_tdTP (failTP `adhocTP` (runGRHSFun fun))) parsed
          runGRHSFun :: (Data a) => (a -> RefactGhc a) -> ParsedGRHSs -> RefactGhc ParsedGRHSs
          runGRHSFun fun grhss@(GHC.GRHSs _ _) = SYB.everywhereM (SYB.mkM fun) grhss
          mzeroOcc = GHC.mkVarOcc "mzero"
          -- nothingOcc = GHC.mkVarOcc "Nothing"
          returnOcc = GHC.mkVarOcc "return"
          -- justOcc = GHC.mkVarOcc "Just"
          replaceNothingAndJust :: GHC.OccName -> RefactGhc GHC.OccName
          replaceNothingAndJust nm
            | (GHC.occNameString nm) == "Nothing" = do
                logm "Replacing nothing"
                return mzeroOcc
            | (GHC.occNameString nm) == "Just" = do
                logm "Replace just"
                return returnOcc
            | otherwise = return nm

replaceBind :: SimpPos -> UnlocParsedHsBind -> RefactGhc ()
replaceBind pos newBind = do
  oldParsed <- getRefactParsed
  let rdrNm = locToRdrName pos oldParsed
  case rdrNm of
    Nothing -> return ()
    (Just (GHC.L _ rNm)) -> do
      newParsed <- SYB.everywhereM (SYB.mkM (worker rNm)) oldParsed
      --logm $ SYB.showData SYB.Parser 3 newParsed
      (liftT getAnnsT) >>= putRefactParsed newParsed
      addMonadImport
#if __GLASGOW_HASKELL__ >= 800
  where worker rNm (funBnd@(GHC.FunBind (GHC.L _ name) _matches _ _ _) :: GHC.HsBind GHC.RdrName)
#else
  where worker rNm (funBnd@(GHC.FunBind (GHC.L _ name) _ _matches _ _ _) :: GHC.HsBind GHC.RdrName)
#endif
          | name == rNm = return newBind
          | otherwise = return funBnd
        worker rNm bind = error $ "replaceBind:unmatched type(rnM,bind):" ++ showGhc (rNm,bind)

--Handles the case where the function can be rewritten with bind.
doRewriteAsBind :: SimpPos -> String -> RefactGhc ()
doRewriteAsBind pos funNm = do
  -- logParsedSource "doRewriteAsBind"
  parsed <- getRefactParsed
  let bind = gfromJust "doRewriteAsBind" $ getHsBind pos parsed
#if __GLASGOW_HASKELL__ >= 800
      matches = GHC.unLoc . GHC.mg_alts . GHC.fun_matches $ bind
#else
      matches = GHC.mg_alts . GHC.fun_matches $ bind
#endif
  if (length matches) > 1
    then error "Multiple matches not supported"
    else do
      let (GHC.L _ match) = head matches
      (varPat, rhs') <- getVarAndRHS match
      (newPat, _) <- liftT $ cloneT varPat
      (newRhs, _) <- liftT $ cloneT rhs'
      let rhs = justToReturn newRhs
      lam <- wrapInLambda newPat rhs
  --    logm $ "New pat: " ++ (SYB.showData SYB.Parser 3 newPat)
#if __GLASGOW_HASKELL__ >= 800
      let (GHC.L _ (GHC.VarPat (GHC.L _ nm))) = newPat
#else
      let (GHC.L _ (GHC.VarPat nm)) = newPat
#endif
          newNm = mkNewNm nm
      new_rhs <- createBindGRHS newNm lam
      replaceGRHS funNm new_rhs newNm

      --logm $ "Final anns: " ++ (show currAnns)
      fixType funNm
      addMonadImport
      prsed <- getRefactParsed
      logExactprint "Final parsed: " prsed
        where mkNewNm rdr = let str = GHC.occNameString $ GHC.rdrNameOcc rdr in
                GHC.Unqual $ GHC.mkVarOcc ("m_" ++ str)

addMonadImport :: RefactGhc ()
addMonadImport = addSimpleImportDecl "Control.Monad" Nothing

--This function finds the function binding and replaces the pattern match.
--The LHS is replaced with the provided name (3rd argument)
--The RHS is replaced with the provided GRHSs
--Asumptions made:
  --Only one LMatch in the match group
  --Only one variable in LHS
replaceGRHS :: String -> (GHC.GRHSs GHC.RdrName (GHC.LHsExpr GHC.RdrName)) -> GHC.RdrName -> RefactGhc ()
replaceGRHS funNm new_rhs lhs_name = do
  parsed <- getRefactParsed
  newParsed <- SYB.everywhereM (SYB.mkM worker) parsed
  --logm $ "new_rhs: " ++ (SYB.showData SYB.Parser 3 new_rhs)
  --logm $ "The new parsed: " ++ (SYB.showData SYB.Parser 3 newParsed)
  (liftT getAnnsT) >>= putRefactParsed newParsed
 -- return ()
    where
          -- rdrName = GHC.Unqual $ GHC.mkDataOcc funNm
          worker :: GHC.HsBind GHC.RdrName -> RefactGhc (GHC.HsBind GHC.RdrName)
#if __GLASGOW_HASKELL__ >= 800
          worker fb@(GHC.FunBind (GHC.L _ nm) _ _ _ _) |
#else
          worker fb@(GHC.FunBind (GHC.L _ nm) _ _ _ _ _) |
#endif
            (GHC.occNameString . GHC.rdrNameOcc) nm == funNm = do
              logm $ "=======Found funbind========"
              new_matches <- SYB.everywhereM (SYB.mkM worker') (GHC.fun_matches fb)
              final_matches <- fix_lhs new_matches
              return $ fb{GHC.fun_matches = final_matches}
          worker bind = return bind
          worker' :: ParsedGRHSs -> RefactGhc ParsedGRHSs
          worker' (GHC.GRHSs _ _) = do
            logm "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! worker'!!!!!!!!!!!!!!!!!!!!!!"
            return new_rhs
          fix_lhs :: ParsedMatchGroup -> RefactGhc ParsedMatchGroup
          fix_lhs mg = do
#if __GLASGOW_HASKELL__ >= 800
            lhs_nameL <- locate lhs_name
            let GHC.L lm [(GHC.L _ match)] = GHC.mg_alts mg
                new_pat = GHC.VarPat lhs_nameL
#else
            let [(GHC.L _ match)] = GHC.mg_alts mg
                new_pat = GHC.VarPat lhs_name
#endif
            lPat <- locate new_pat
            addAnnVal lPat
            let newMatch = match {GHC.m_pats = [lPat]}
                mAnn = annNone {annsDP = [(G GHC.AnnEqual, (DP (0,1)))]}
            new_l_match <- locate newMatch
            addAnn new_l_match mAnn
#if __GLASGOW_HASKELL__ >= 800
            return $ mg {GHC.mg_alts = GHC.L lm [new_l_match]}
#else
            return $ mg {GHC.mg_alts = [new_l_match]}
#endif

-- This creates a GRHS of the form "name >>= expr" and adds the appropriate annotations, returns the GRHSs.
createBindGRHS :: GHC.RdrName -> GHC.LHsExpr GHC.RdrName -> RefactGhc (GHC.GRHSs GHC.RdrName (GHC.LHsExpr GHC.RdrName))
createBindGRHS name lam_par = do
#if __GLASGOW_HASKELL__ >= 800
  bindL <- locate (GHC.Unqual (GHC.mkDataOcc ">>="))
  liftT $ addSimpleAnnT bindL (DP (0,1)) [((G GHC.AnnVal),DP (0,0))]
  bind_occ <- locate $ GHC.HsVar bindL
#else
  bind_occ <- locate $ GHC.HsVar (GHC.Unqual (GHC.mkDataOcc ">>="))
#endif
  let occDp = [(G GHC.AnnVal, DP (0,1))]
      occAnn = annNone {annsDP = occDp}
  addAnn bind_occ occAnn
#if __GLASGOW_HASKELL__ >= 800
  nameL <- locate name
  liftT $ addSimpleAnnT nameL (DP (0,1)) [((G GHC.AnnVal),DP (0,0))]
  l_name <- locate $ GHC.HsVar nameL
#else
  l_name <- locate $ GHC.HsVar name
#endif
  let l_ann = annNone {annsDP = [(G GHC.AnnVal, DP (0,1))]}
  addAnn l_name l_ann
  oppApp <- locate $ GHC.OpApp l_name bind_occ GHC.PlaceHolder lam_par
  addEmptyAnn oppApp
  lgrhs <- locate $ GHC.GRHS [] oppApp
  addEmptyAnn lgrhs
#if __GLASGOW_HASKELL__ >= 800
  return $ GHC.GRHSs [lgrhs] (GHC.noLoc GHC.EmptyLocalBinds)
#else
  return $ GHC.GRHSs [lgrhs] GHC.EmptyLocalBinds
#endif

--This takes an AST chunk traverses it and changes any calls to the "Just" constructor to "return"
justToReturn :: (Data a) => a -> a
justToReturn ast = SYB.everywhere (SYB.mkT worker) ast
  where worker :: GHC.OccName -> GHC.OccName
        worker nm = let just = GHC.mkDataOcc "Just" in
          if nm == just
          then GHC.mkDataOcc "return"
          else nm

{-

--Takes a single match and returns a tuple containing the grhs and the pattern
--Assumptions:
  -- Only a single pattern will be returned. Which pattern is returned depends on the behaviour of SYB.something.
getVarAndRHS :: GHC.Match GHC.RdrName (GHC.LHsExpr GHC.RdrName) -> RefactGhc (GHC.LPat GHC.RdrName, ParsedGRHSs)
getVarAndRHS match = do
  let (Just pat) = SYB.something (Nothing `SYB.mkQ` varPat) (GHC.m_pats match)
  return (pat , GHC.m_grhss match)
    where varPat lPat@(GHC.L _ (GHC.VarPat _ )) = Just lPat
          varPat _ = Nothing


--Looks up the function binding at the given position. Returns nothing if the position does not contain a binding.
getHsBind :: (Data a) => SimpPos -> String -> a -> Maybe (GHC.HsBind GHC.RdrName)
getHsBind pos funNm a =
  let rdrNm = locToRdrName pos a in
  case rdrNm of
  Nothing -> Nothing
  (Just (GHC.L _ rNm)) -> SYB.everythingStaged SYB.Parser (<|>) Nothing (Nothing `SYB.mkQ` isBind) a
    where isBind (bnd@(GHC.FunBind (GHC.L _ name) _ _ _ _ _) :: GHC.HsBind GHC.RdrName)
            | name == rNm = (Just bnd)
          isBind _ = Nothing
-}

--This function takes in the name of a function and determines if the binding contains the case "Nothing = Nothing"
--If the Nothing to Nothing case is found then it is removed from the parsed source.
containsNothingToNothing :: String -> Int -> SimpPos -> GHC.HsBind GHC.RdrName -> RefactGhc Bool
containsNothingToNothing funNm argNum pos a = do
  -- dFlags <- GHC.getSessionDynFlags
  parsed <- getRefactParsed
  let
      -- nToNStr = funNm ++ " Nothing = Nothing"
      bind = gfromJust "containsNothingToNothing" $ getHsBind pos parsed
      mg = GHC.fun_matches bind
#if __GLASGOW_HASKELL__ >= 800
      -- (GHC.L _ alt) = (GHC.unLoc $ GHC.mg_alts mg) !! 0
#else
      -- (GHC.L _ alt) = (GHC.mg_alts mg) !! 0
#endif
--  logm $ "mg_alts: " ++ (SYB.showData SYB.Parser 3 alt)
--  logm $ show $ length (GHC.m_pats alt)
  let oldMatches = SYB.everything (++) ([] `SYB.mkQ` isNtoNMatch) bind
  if (length oldMatches == 0)
    then return False
    else do
#if __GLASGOW_HASKELL__ >= 800
    let GHC.L lm ms = GHC.mg_alts mg
        newMs = getNewMs oldMatches ms
        newMg = mg {GHC.mg_alts = GHC.L lm newMs}
#else
    let newMs = getNewMs oldMatches (GHC.mg_alts mg)
        newMg = mg {GHC.mg_alts = newMs}
#endif
        newBind = bind {GHC.fun_matches = newMg}
    removeMatches pos newBind oldMatches
    return True
  where
    isNtoNMatch :: ParsedLMatch -> [ParsedLMatch]
    isNtoNMatch lm@(GHC.L _ m) =
      let rhsCheck = checkGRHS $ GHC.m_grhss m
          lhsCheck = checkPats $ GHC.m_pats m in
      if (rhsCheck && lhsCheck)
        then [lm]
        else []
    checkGRHS :: ParsedGRHSs -> Bool
    checkGRHS (GHC.GRHSs [(GHC.L _ (GHC.GRHS _ (GHC.L _ body)))] _)  = isNothingVar body
    checkGRHS _ = False
    checkPats :: [GHC.LPat GHC.RdrName] -> Bool
    checkPats patLst =
      if argNum <= length patLst
      then let (GHC.L _ pat) = patLst !! (argNum - 1) in
      isNothingPat pat
      else False
    filterMatch :: ParsedLMatch -> [ParsedLMatch] -> [ParsedLMatch]
    filterMatch (GHC.L l1 _) = filter (\(GHC.L l2 _) -> l1 /= l2)
    getNewMs :: [ParsedLMatch] -> [ParsedLMatch] -> [ParsedLMatch]
    getNewMs [] lst = lst
    getNewMs (m:ms) lst = let newLst = filterMatch m lst in
      getNewMs ms newLst
    isNothingPat :: GHC.Pat GHC.RdrName -> Bool
#if __GLASGOW_HASKELL__ >= 800
    isNothingPat (GHC.VarPat (GHC.L _ nm)) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#else
    isNothingPat (GHC.VarPat nm) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#endif
    isNothingPat (GHC.ConPatIn (GHC.L _ nm) _) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
    isNothingPat _ = False
    isNothingVar :: GHC.HsExpr GHC.RdrName -> Bool
#if __GLASGOW_HASKELL__ >= 800
    isNothingVar (GHC.HsVar (GHC.L _ nm)) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#else
    isNothingVar (GHC.HsVar nm) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#endif
    isNothingVar _ = False

-- Removes the given matches from the given binding. Uses the position to retrieve the rdrName.
removeMatches :: SimpPos -> GHC.HsBind GHC.RdrName -> [GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName)] -> RefactGhc ()
removeMatches pos newBind matches = do
  parsed <- getRefactParsed
  let rdrNm = gfromJust "Couldn't get rdrName in removeMatch" $ locToRdrName pos parsed
  newParsed <- SYB.everywhereMStaged SYB.Parser (SYB.mkM (replaceBind rdrNm)) parsed
  -- currAnns <- fetchAnnsFinal
  mapM_ removeAnns matches
  (liftT getAnnsT) >>= putRefactParsed newParsed
  return ()
    where replaceBind :: GHC.Located GHC.RdrName -> GHC.HsBind GHC.RdrName -> RefactGhc (GHC.HsBind GHC.RdrName)
#if __GLASGOW_HASKELL__ >= 800
          replaceBind rdrNm ((GHC.FunBind name _ _ _ _) :: GHC.HsBind GHC.RdrName)
#else
          replaceBind rdrNm ((GHC.FunBind name _ _ _ _ _) :: GHC.HsBind GHC.RdrName)
#endif
            | name == rdrNm = return newBind
          replaceBind _ a = return a

--This function is very specific to Maybe to MonadPlus refactoring. It rewrites the type signature so that the calls to maybe will be replaced with type variable "m"
--and adds the MonadPlus type class constraint to m
--Assumptions:
  --Assumes the function is of type Maybe a -> Maybe a
  --
  -- Should refactor to take in the argNum parameter and fix the type depending on that

fixType' :: String -> Int -> RefactGhc ()
fixType' funNm argPos = do
  logm "Fixing type"
  parsed <- getRefactParsed
  let m_sig = getSigD funNm parsed
      (GHC.L sigL (GHC.SigD sig)) = gfromJust "fixType'" m_sig
  fixedClass <- fixTypeClass sig
  --This needs to be fixed to replace only the correct argument and output type
  replacedMaybe <- replaceMaybeWithVariable fixedClass
  newSig <- locate (GHC.SigD replacedMaybe)
  addNewKeyword ((G GHC.AnnDcolon), DP (0,1)) newSig
  logm $ "Span: " ++ show sigL
  newParsed <- replaceAtLocation sigL newSig
  synthesizeAnns newSig
  addNewLines 2 newSig
  anns <- liftT getAnnsT
  logm $ showAnnData anns 3 newParsed
  putRefactParsed newParsed anns
    where replaceMaybeWithVariable :: GHC.Sig GHC.RdrName -> RefactGhc (GHC.Sig GHC.RdrName)
          replaceMaybeWithVariable sig = SYB.everywhereM (SYB.mkM worker) sig
#if __GLASGOW_HASKELL__ >= 802
            where worker tyVar@(GHC.HsTyVar p (GHC.L lv rdrName))
                    | compNames "Maybe" rdrName = let newRdr = (GHC.mkVarUnqual . fsLit) "m" in
                        return (GHC.HsTyVar p (GHC.L lv newRdr))
                    | otherwise = return tyVar
#elif __GLASGOW_HASKELL__ >= 800
            where worker tyVar@(GHC.HsTyVar (GHC.L lv rdrName))
                    | compNames "Maybe" rdrName = let newRdr = (GHC.mkVarUnqual . fsLit) "m" in
                        return (GHC.HsTyVar (GHC.L lv newRdr))
                    | otherwise = return tyVar
#else
            where worker tyVar@(GHC.HsTyVar rdrName)
                    | compNames "Maybe" rdrName = let newRdr = (GHC.mkVarUnqual . fsLit) "m" in
                        return (GHC.HsTyVar newRdr)
                    | otherwise = return tyVar
#endif
                  worker var = return var
          fixTypeClass :: GHC.Sig GHC.RdrName -> RefactGhc (GHC.Sig GHC.RdrName)
#if __GLASGOW_HASKELL__ >= 802
          fixTypeClass sig@(GHC.TypeSig names (GHC.HsWC wcs (GHC.HsIB a (GHC.L lt hsType) b))) =
            case hsType of
              (GHC.HsQualTy context ty) -> do
                newContext <- case context of
                                (GHC.L _ []) -> do
                                  tyCls <- genMonadPlusClass
                                  parTy <- locate (GHC.HsParTy tyCls)
                                  addNewKeyword ((G GHC.AnnCloseP),DP (0,0)) parTy
                                  liftT $ setPrecedingLinesT parTy 0 (-1)
                                  lList <- locate [parTy]
                                  addNewKeywords [((G GHC.AnnOpenP), DP (0,0)),((G GHC.AnnCloseP), DP (0,-1)),((G GHC.AnnDarrow), DP (0,1))] lList
                                  return lList
                                (GHC.L _ [(GHC.L _ (GHC.HsParTy innerTy))]) -> do
                                  tyCls <- genMonadPlusClass
                                  lList <- locate [innerTy,tyCls]
                                  return lList
                                (GHC.L _ lst) -> do
                                  tyCls <- genMonadPlusClass
                                  lList <- locate (tyCls:lst)
                                  return lList
                newForAll <- locate (GHC.HsQualTy newContext ty)
                liftT $ setPrecedingLinesT ty 0 1
                liftT $ setPrecedingLinesT newForAll 0 1
                return (GHC.TypeSig names (GHC.HsWC wcs (GHC.HsIB a newForAll b)))
              (GHC.HsForAllTy bndrs ty) -> do
                newContext <- do
                                  tyCls <- genMonadPlusClass
                                  parTy <- locate (GHC.HsParTy tyCls)
                                  addNewKeyword ((G GHC.AnnCloseP),DP (0,0)) parTy
                                  liftT $ setPrecedingLinesT parTy 0 (-1)
                                  lList <- locate [parTy]
                                  addNewKeywords [((G GHC.AnnOpenP), DP (0,0)),((G GHC.AnnCloseP), DP (0,-1)),((G GHC.AnnDarrow), DP (0,1))] lList
                                  return lList
                -- newForAll <- locate (GHC.HsForAllTy b ty)
                newForAll <- locate (GHC.HsQualTy newContext ty)
                liftT $ setPrecedingLinesT ty 0 1
                liftT $ setPrecedingLinesT newForAll 0 1
                -- return (GHC.TypeSig names newForAll p)
                return (GHC.TypeSig names (GHC.HsWC wcs (GHC.HsIB a newForAll b)))
              unexpected -> do
                logDataWithAnns "fixTypeClass:unexpected" unexpected
                newContext <- do
                                  tyCls <- genMonadPlusClass
                                  parTy <- locate (GHC.HsParTy tyCls)
                                  -- addNewKeyword ((G GHC.AnnCloseP),DP (0,0)) parTy
                                  liftT $ setPrecedingLinesT parTy 0 (-1)
                                  lList <- locate [parTy]
                                  addNewKeywords [((G GHC.AnnOpenP), DP (0,1)),((G GHC.AnnCloseP), DP (0,0)),((G GHC.AnnDarrow), DP (0,1))] lList
                                  return lList
                let qualTy = GHC.HsQualTy newContext (GHC.L lt hsType)
                qualTyL <- locate qualTy
                return (GHC.TypeSig names (GHC.HsWC wcs (GHC.HsIB a qualTyL b)))
#elif __GLASGOW_HASKELL__ >= 800
          -- fixTypeClass sig@(GHC.TypeSig names (GHC.HsWC pns wcs (GHC.HsIB a (GHC.L lt hsType)))) =
          fixTypeClass sig@(GHC.TypeSig names (GHC.HsIB pvs (GHC.HsWC pns wcs (GHC.L lt hsType)))) =
            case hsType of
              (GHC.HsQualTy context ty) -> do
                newContext <- case context of
                                (GHC.L _ []) -> do
                                  tyCls <- genMonadPlusClass
                                  parTy <- locate (GHC.HsParTy tyCls)
                                  addNewKeyword ((G GHC.AnnCloseP),DP (0,0)) parTy
                                  liftT $ setPrecedingLinesT parTy 0 (-1)
                                  lList <- locate [parTy]
                                  addNewKeywords [((G GHC.AnnOpenP), DP (0,0)),((G GHC.AnnCloseP), DP (0,-1)),((G GHC.AnnDarrow), DP (0,1))] lList
                                  return lList
                                (GHC.L _ [(GHC.L _ (GHC.HsParTy innerTy))]) -> do
                                  tyCls <- genMonadPlusClass
                                  lList <- locate [innerTy,tyCls]
                                  return lList
                                (GHC.L _ lst) -> do
                                  tyCls <- genMonadPlusClass
                                  lList <- locate (tyCls:lst)
                                  return lList
                newForAll <- locate (GHC.HsQualTy newContext ty)
                liftT $ setPrecedingLinesT ty 0 1
                liftT $ setPrecedingLinesT newForAll 0 1
                -- return (GHC.TypeSig names (GHC.HsWC wcs (GHC.HsIB a newForAll)))
                return (GHC.TypeSig names (GHC.HsIB pvs (GHC.HsWC pns wcs newForAll)))
              (GHC.HsForAllTy bndrs ty) -> do
                newContext <- do
                                  tyCls <- genMonadPlusClass
                                  parTy <- locate (GHC.HsParTy tyCls)
                                  addNewKeyword ((G GHC.AnnCloseP),DP (0,0)) parTy
                                  liftT $ setPrecedingLinesT parTy 0 (-1)
                                  lList <- locate [parTy]
                                  addNewKeywords [((G GHC.AnnOpenP), DP (0,0)),((G GHC.AnnCloseP), DP (0,-1)),((G GHC.AnnDarrow), DP (0,1))] lList
                                  return lList
                -- newForAll <- locate (GHC.HsForAllTy b ty)
                newForAll <- locate (GHC.HsQualTy newContext ty)
                liftT $ setPrecedingLinesT ty 0 1
                liftT $ setPrecedingLinesT newForAll 0 1
                -- return (GHC.TypeSig names (GHC.HsWC pns wcs (GHC.HsIB a newForAll b)))
                return (GHC.TypeSig names (GHC.HsIB pvs (GHC.HsWC pns wcs newForAll)))
              unexpected -> do
                logDataWithAnns "fixTypeClass:unexpected" unexpected
                newContext <- do
                                  tyCls <- genMonadPlusClass
                                  parTy <- locate (GHC.HsParTy tyCls)
                                  -- addNewKeyword ((G GHC.AnnCloseP),DP (0,0)) parTy
                                  liftT $ setPrecedingLinesT parTy 0 (-1)
                                  lList <- locate [parTy]
                                  addNewKeywords [((G GHC.AnnOpenP), DP (0,1)),((G GHC.AnnCloseP), DP (0,0)),((G GHC.AnnDarrow), DP (0,1))] lList
                                  return lList
                let qualTy = GHC.HsQualTy newContext (GHC.L lt hsType)
                qualTyL <- locate qualTy
                -- return (GHC.TypeSig names (GHC.HsWC wcs (GHC.HsIB a qualTyL b)))
                return (GHC.TypeSig names (GHC.HsIB pvs (GHC.HsWC pns wcs qualTyL)))
#else
          fixTypeClass sig@(GHC.TypeSig names (GHC.L _ hsType) p) =
            case hsType of
              (GHC.HsForAllTy f m b context ty) -> do
                newContext <- case context of
                                (GHC.L _ []) -> do
                                  tyCls <- genMonadPlusClass
                                  parTy <- locate (GHC.HsParTy tyCls)
                                  addNewKeyword ((G GHC.AnnCloseP),DP (0,0)) parTy
                                  liftT $ setPrecedingLinesT parTy 0 (-1)
                                  lList <- locate [parTy]
                                  addNewKeywords [((G GHC.AnnOpenP), DP (0,0)),((G GHC.AnnCloseP), DP (0,-1)),((G GHC.AnnDarrow), DP (0,1))] lList
                                  return lList
                                (GHC.L _ [(GHC.L _ (GHC.HsParTy innerTy))]) -> do
                                  tyCls <- genMonadPlusClass
                                  lList <- locate [innerTy,tyCls]
                                  return lList
                                (GHC.L _ lst) -> do
                                  tyCls <- genMonadPlusClass
                                  lList <- locate (tyCls:lst)
                                  return lList
                newForAll <- locate (GHC.HsForAllTy f m b newContext ty)
                liftT $ setPrecedingLinesT ty 0 1
                liftT $ setPrecedingLinesT newForAll 0 1
                return (GHC.TypeSig names newForAll p)
#endif

          genMonadPlusClass :: RefactGhc (GHC.LHsType GHC.RdrName)
          genMonadPlusClass = do
            let mPlusNm = GHC.mkVarUnqual (fsLit "MonadPlus")
                mNm     = GHC.mkVarUnqual (fsLit "m")
#if __GLASGOW_HASKELL__ >= 802
            mPlusNmL <- locate mPlusNm
            addAnnValWithDP mPlusNmL (DP (0,0))
            lPlus <- locate (GHC.HsTyVar GHC.NotPromoted mPlusNmL)
#elif __GLASGOW_HASKELL__ >= 800
            mPlusNmL <- locate mPlusNm
            addAnnValWithDP mPlusNmL (DP (0,0))
            lPlus <- locate (GHC.HsTyVar mPlusNmL)
#else
            lPlus <- locate (GHC.HsTyVar mPlusNm)
            addAnnVal lPlus
#endif
            liftT $ setPrecedingLinesT lPlus 0 0
#if __GLASGOW_HASKELL__ >= 802
            mNmL <- locate mNm
            addAnnVal mNmL
            lM <- locate (GHC.HsTyVar GHC.NotPromoted mNmL)
#elif __GLASGOW_HASKELL__ >= 800
            mNmL <- locate mNm
            addAnnVal mNmL
            lM <- locate (GHC.HsTyVar mNmL)
#else
            lM <- locate (GHC.HsTyVar mNm)
            addAnnVal lM
#endif
            lApp <- locate (GHC.HsAppTy lPlus lM)
            return lApp


fixType :: String -> RefactGhc ()
fixType funNm = do
  parsed <- getRefactParsed
  currAnns <- fetchAnnsFinal
  dFlags <- GHC.getSessionDynFlags
  logm $ "fixType:funNm=" ++ funNm
  logParsedSource "fixType"
  let m_sig = getSigD funNm parsed
      (GHC.L sigL (GHC.SigD sig)) = gfromJust "fixType: getting sig" m_sig
      iType = gfromJust "fixType: iType" $ getInnerType sig
      strTy = exactPrint iType currAnns
      tyStr = funNm ++ " :: (MonadPlus m) => m " ++ strTy ++ " -> m " ++ strTy
      pRes = parseDecl dFlags "MaybeToMonadPlus.hs" tyStr
  logm $ "Type string:strTy " ++ strTy
  logm $ "Type string:tyStr " ++ tyStr
  -- logDataWithAnns "fixType:sig" sig
  (anns, newSig) <- handleParseResult "MaybeToMonadPlus.fixType" pRes
  newParsed <- replaceAtLocation sigL newSig
  let newAnns = Map.union currAnns anns
  putRefactParsed newParsed newAnns
  addNewLines 2 newSig

getSigD :: (Data a) => String -> a -> Maybe (GHC.LHsDecl GHC.RdrName)
getSigD funNm = SYB.something (Nothing `SYB.mkQ` isSigD)
  where
    isSigD :: GHC.LHsDecl GHC.RdrName -> Maybe (GHC.LHsDecl GHC.RdrName)
    isSigD s@(GHC.L _ (GHC.SigD sig)) = if isSig sig
                                        then Just s
                                        else Nothing
    isSigD _ = Nothing
    isSig :: GHC.Sig GHC.RdrName -> Bool
#if __GLASGOW_HASKELL__ >= 800
    isSig sig@(GHC.TypeSig [(GHC.L _ nm)] _) = (compNames funNm nm)
#else
    isSig sig@(GHC.TypeSig [(GHC.L _ nm)] _ _) = (compNames funNm nm)
#endif
    isSig _ = False

compNames :: String -> GHC.RdrName -> Bool
compNames s rdr = let sRdr = (GHC.occNameString . GHC.rdrNameOcc) rdr in
  sRdr == s

getInnerType :: GHC.Sig GHC.RdrName -> Maybe (GHC.LHsType GHC.RdrName)
getInnerType = SYB.everything (<|>) (Nothing `SYB.mkQ` getTy)
#if __GLASGOW_HASKELL__ >= 802
  where getTy :: GHC.HsType GHC.RdrName -> Maybe (GHC.LHsType GHC.RdrName)
        getTy (GHC.HsAppsTy [GHC.L _ (GHC.HsAppPrefix mCon), GHC.L _ (GHC.HsAppPrefix otherTy)])
          = if isMaybeTy mCon
                                             then Just otherTy
                                             else Nothing
        getTy _ = Nothing

        isMaybeTy :: GHC.LHsType GHC.RdrName -> Bool
        isMaybeTy (GHC.L _ (GHC.HsTyVar _ (GHC.L _ (GHC.Unqual occNm)))) = (GHC.occNameString occNm) == "Maybe"
        isMaybeTy _ = False
#elif __GLASGOW_HASKELL__ >= 800
  where getTy :: GHC.HsType GHC.RdrName -> Maybe (GHC.LHsType GHC.RdrName)
        getTy (GHC.HsAppsTy [GHC.L _ (GHC.HsAppPrefix mCon), GHC.L _ (GHC.HsAppPrefix otherTy)])
          = if isMaybeTy mCon
                                             then Just otherTy
                                             else Nothing
        getTy _ = Nothing

        isMaybeTy :: GHC.LHsType GHC.RdrName -> Bool
        isMaybeTy (GHC.L _ (GHC.HsTyVar (GHC.L _ (GHC.Unqual occNm)))) = (GHC.occNameString occNm) == "Maybe"
        isMaybeTy _ = False
#else
  where getTy :: GHC.HsType GHC.RdrName -> Maybe (GHC.LHsType GHC.RdrName)
        getTy (GHC.HsAppTy mCon otherTy) = if isMaybeTy mCon
                                             then Just otherTy
                                             else Nothing
        getTy _ = Nothing

        isMaybeTy :: GHC.LHsType GHC.RdrName -> Bool
        isMaybeTy (GHC.L _ (GHC.HsTyVar (GHC.Unqual occNm))) = (GHC.occNameString occNm) == "Maybe"
        isMaybeTy _ = False
#endif

replaceAtLocation :: (Data a) => GHC.SrcSpan -> GHC.Located a -> RefactGhc (GHC.ParsedSource)
replaceAtLocation span new = do
  logm $ "Span: " ++ (show span)
  parsed <- getRefactParsed
  newParsed <- SYB.everywhereM (SYB.mkM findLoc) parsed
  return newParsed
    where --findLoc :: (forall b. (Data b) => GHC.Located b -> RefactGhc (GHC.Located b))
          findLoc a@(GHC.L l s) = if l == span
                                  then do
                                    removeAnns s
                                    return new
                                  else return a
