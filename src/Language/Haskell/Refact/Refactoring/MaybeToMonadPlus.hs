{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Refact.Refactoring.MaybeToMonadPlus
  (
    maybeToMonadPlus
  , compMaybeToMonadPlus
  ) where

-- import qualified BasicTypes as GHC
import qualified FastString as GHC
import qualified GHC        as GHC
import qualified OccName    as GHC
import qualified RdrName    as GHC
import qualified Type       as GHC

import           Data.Generics as SYB

import           Control.Applicative
import           Data.Generics.Strafunski.StrategyLib.StrategyLib
import qualified Haskell.Ide.Engine.PluginApi as HIE (BiosOptions(..))
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers
import           Language.Haskell.GHC.ExactPrint.Types
import           Language.Haskell.GHC.ExactPrint.Utils
import           Language.Haskell.Refact.API
-- import           System.Directory

-- ---------------------------------------------------------------------

maybeToMonadPlus :: RefactSettings -> HIE.BiosOptions -> FilePath -> SimpPos -> Int -> IO [FilePath]
maybeToMonadPlus settings cradle fileName pos argNum = do
  absFileName <- normaliseFilePath fileName
  runRefacSession settings cradle (compMaybeToMonadPlus absFileName pos argNum)

compMaybeToMonadPlus :: FilePath -> SimpPos -> Int -> RefactGhc [ApplyRefacResult]
compMaybeToMonadPlus fileName pos argNum = do
  (refRes@((_fp,ismod), _),()) <- applyRefac (doMaybeToPlus pos argNum) (RSFile fileName)
  case ismod of
    RefacUnmodifed -> error "Maybe to MonadPlus synonym failed"
    RefacModified -> return ()
  return [refRes]

-- ---------------------------------------------------------------------

-- TODO:AZ do not compare RdrName's, use Name's instead

-- TODO:AZ simplify real world use, require the pos to be on the parameter to be
--         changed

-- | This refactoring tries to generalise something of type Maybe to become
-- either of type 'Monad' or 'MonadPlus'. The implementation of this refactoring
-- attempts to produce the “most general" version of the source program. This
-- means that when possible the refactoring will replace the targetted 'Maybe'
-- type with a value of type 'Monad', if this is not possible the value will be
-- of type 'MonadPlus', and finally if that also isn’t possible the refactoring
-- cannot continue and the source and target programs of the refactoring are
-- identical.
doMaybeToPlus :: SimpPos -> Int -> RefactGhc ()
doMaybeToPlus pos argNum = do
  parsed <- getRefactParsed
  -- TODO:Add test that position defines function with name `funNm`
  let mBind = getHsBind pos parsed
  case mBind of
    Nothing -> error "Function bind not found"
    Just funBind -> do
      nm <- getRefactNameMap
#if __GLASGOW_HASKELL__ >= 806
      let funNm = ghead "doMaybeToPlus" (definedNamesRdr nm (GHC.noLoc (GHC.ValD GHC.noExt funBind)))
#else
      let funNm = ghead "doMaybeToPlus" (definedNamesRdr nm (GHC.noLoc (GHC.ValD funBind)))
#endif
      -- Most general: contains Nothing to Nothing, can be Monadic
      -- otherwise canReplaceConstructors coming up true means MonadPlus
      hasNtoN <- containsNothingToNothing argNum pos funBind
      -- logm $ "Result of searching for nothing to nothing: " ++ (show hasNtoN)
      case hasNtoN of
        Just newBind -> do
          logm "Converting to Monad"
          doRewriteAsBind newBind funNm
        Nothing -> do
          canReplaceConstructors <- isOutputType argNum pos
          logm $ "Result of canReplaceConstructors: " ++ (show canReplaceConstructors)
          if canReplaceConstructors
            then do
              logm "Converting to MonadPlus"
              replaceConstructors pos funNm argNum
            else
              -- AZ:TODO should this be an error, to cause feedback to the user?
              logm "Cannot perform conversion"

-- | This checks if the argument to be refactored is the output type. If this is
-- true then the refactoring will just consist of replacing all RHS calls to the
-- Maybe type with their MPlus equivalents. I need some way of checking if the
-- type
isOutputType :: Int -> SimpPos -> RefactGhc Bool
isOutputType argNum pos = do
  parsed <- getRefactParsed
  (Just name) <- locToName pos parsed
  (Just ty)   <- getTypeForName name
  -- logDataWithAnns "isOutputType:ty" ty
  let depth = typeDepth ty
  logm $ "isOutputType:depth=" ++ show depth
  return $ depth == argNum
    where typeDepth :: GHC.Type -> Int
          typeDepth ty = case (GHC.isFunTy ty) of
            True  -> 1 + typeDepth (GHC.funResultTy ty)
            False -> 1

-- | This handles the case where only the output type of the function is being
-- modified so calls to Nothing and Just can be replaced with mzero and return
-- respectively in GRHSs
replaceConstructors :: SimpPos -> GHC.Name -> Int -> RefactGhc ()
replaceConstructors pos funNm argNum = do
  parsed <- getRefactParsed
  let (Just bind) = getHsBind pos parsed
  newBind <- applyInGRHSs bind replaceNothingAndJust
  replaceBind pos newBind
  fixType' funNm argNum
    where applyInGRHSs :: (Data a) => GHC.HsBind GhcPs -> (a -> RefactGhc a) -> RefactGhc (GHC.HsBind GhcPs)
          applyInGRHSs bind fun = applyTP (stop_tdTP (failTP `adhocTP` (runGRHSFun fun))) bind

          runGRHSFun :: (Data a) => (a -> RefactGhc a) -> ParsedGRHSs -> RefactGhc ParsedGRHSs
          runGRHSFun fun grhss@(GHC.GRHSs {}) = SYB.everywhereM (SYB.mkM fun) grhss
#if __GLASGOW_HASKELL__ >= 806
          runGRHSFun _ grhss@(GHC.XGRHSs {}) = return grhss
#endif

          mzeroOcc = GHC.mkVarOcc "mzero"
          returnOcc = GHC.mkVarOcc "return"

          replaceNothingAndJust :: GHC.OccName -> RefactGhc GHC.OccName
          replaceNothingAndJust nm
            | GHC.occNameString nm == "Nothing" = do
                logm "Replacing nothing"
                return mzeroOcc
            | GHC.occNameString nm == "Just" = do
                logm "Replace just"
                return returnOcc
            | otherwise = return nm

replaceBind :: SimpPos -> GHC.HsBind GhcPs -> RefactGhc ()
replaceBind pos newBind = do
  oldParsed <- getRefactParsed
  let rdrNm = locToRdrName pos oldParsed
  case rdrNm of
    Nothing -> return ()
    (Just (GHC.L _ rNm)) -> do
      newParsed <- SYB.everywhereM (SYB.mkM (worker rNm)) oldParsed
      --logm $ SYB.showData SYB.Parser 3 newParsed
      putRefactParsed newParsed mempty
      addMonadImport
  where worker rNm (funBnd@(GHC.FunBind { GHC.fun_id = (GHC.L _ name) }) :: GHC.HsBind GhcPs)
          | name == rNm = return newBind
          | otherwise   = return funBnd
        worker rNm bind = error $ "replaceBind:unmatched type(rnM,bind):" ++ showGhc (rNm,bind)

-- | Handles the case where the function can be rewritten with 'bind'.
-- i.e. Conversion to a fully general Monad
doRewriteAsBind :: GHC.HsBind GhcPs -> GHC.Name -> RefactGhc ()
-- doRewriteAsBind :: GHC.HsBind GhcPs -> String -> RefactGhc (GHC.HsBind GhcPs)
doRewriteAsBind bind funNm = do
#if __GLASGOW_HASKELL__ >= 800
  let matches = GHC.unLoc . GHC.mg_alts . GHC.fun_matches $ bind
#else
  let matches = GHC.mg_alts . GHC.fun_matches $ bind
#endif
  if length matches > 1
    then error "Multiple matches not supported"
    else do
      let (GHC.L _ match) = head matches
      (varPat, rhs') <- getVarAndRHS match
      (newPat, _) <- liftT $ cloneT varPat
      (newRhs, _) <- liftT $ cloneT rhs'
      let rhs = justToReturn newRhs
      lam <- wrapInLambda newPat rhs
  --    logm $ "New pat: " ++ (SYB.showData SYB.Parser 3 newPat)
#if __GLASGOW_HASKELL__ >= 806
      let (LL _ (GHC.VarPat _ (GHC.L _ nm))) = newPat
#elif __GLASGOW_HASKELL__ >= 800
      let (GHC.L _ (GHC.VarPat (GHC.L _ nm))) = newPat
#else
      let (GHC.L _ (GHC.VarPat nm)) = newPat
#endif
          -- TODO:AZ: we need to check that this name is unique, within the existing params
          newNm = mkRdrName ("m_" ++ (showGhc nm))
      new_rhs <- createBindGRHS newNm lam
      replaceGRHS funNm new_rhs newNm

      --logm $ "Final anns: " ++ (show currAnns)
      fixType funNm
      addMonadImport
      -- logParsedSource "Final parsed: "

addMonadImport :: RefactGhc ()
addMonadImport = addSimpleImportDecl "Control.Monad" Nothing

-- ---------------------------------------------------------------------

-- | This function finds the function binding and replaces the pattern match.
-- The LHS is replaced with the provided name (3rd argument)
-- The RHS is replaced with the provided GRHSs
-- Asumptions made:
--  Only one LMatch in the match group
--  Only one variable in LHS
replaceGRHS :: GHC.Name -> GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs) -> GHC.RdrName -> RefactGhc ()
replaceGRHS funNm new_rhs lhs_name = do
  nm <- getRefactNameMap
  parsed <- getRefactParsed
  newParsed <- SYB.everywhereM (SYB.mkM (worker nm)) parsed
  --logm $ "new_rhs: " ++ (SYB.showData SYB.Parser 3 new_rhs)
  --logm $ "The new parsed: " ++ (SYB.showData SYB.Parser 3 newParsed)
  putRefactParsed newParsed mempty
 -- return ()
    where
          -- rdrName = GHC.Unqual $ GHC.mkDataOcc funNm
          worker :: NameMap ->  GHC.HsBind GhcPs -> RefactGhc (GHC.HsBind GhcPs)
          worker nm fb@(GHC.FunBind { GHC.fun_id = ln })
            -- *| (GHC.occNameString . GHC.rdrNameOcc) nm == funNm = do
            | sameName nm ln funNm = do
              logm $ "=======Found funbind========"
              new_matches <- SYB.everywhereM (SYB.mkM worker') (GHC.fun_matches fb)
              final_matches <- fix_lhs new_matches
              return $ fb{GHC.fun_matches = final_matches}
          worker _ bind = return bind

          worker' :: ParsedGRHSs -> RefactGhc ParsedGRHSs
          worker' (GHC.GRHSs {}) = do
            logm "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! worker'!!!!!!!!!!!!!!!!!!!!!!"
            return new_rhs
#if __GLASGOW_HASKELL__ >= 806
          worker' (GHC.XGRHSs {}) = error "MaybeToMonadPlus.worker"
#endif

          fix_lhs :: ParsedMatchGroup -> RefactGhc ParsedMatchGroup
          fix_lhs mg = do
#if __GLASGOW_HASKELL__ >= 806
            lhs_nameL <- locate lhs_name
            let GHC.L lm [(GHC.L _ match)] = GHC.mg_alts mg
                new_pat = GHC.VarPat GHC.noExt lhs_nameL
#elif __GLASGOW_HASKELL__ >= 800
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
            liftT $ addAnn new_l_match mAnn
#if __GLASGOW_HASKELL__ >= 800
            return $ mg {GHC.mg_alts = GHC.L lm [new_l_match]}
#else
            return $ mg {GHC.mg_alts = [new_l_match]}
#endif

-- | This creates a GRHS of the form "name >>= expr" and adds the appropriate annotations, returns the GRHSs.
createBindGRHS :: GHC.RdrName -> GHC.LHsExpr GhcPs -> RefactGhc (GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs))
createBindGRHS name lam_par = do
#if __GLASGOW_HASKELL__ >= 806
  bindL <- locate (GHC.Unqual (GHC.mkDataOcc ">>="))
  liftT $ addSimpleAnnT bindL (DP (0,1)) [((G GHC.AnnVal),DP (0,0))]
  bind_occ <- locate $ GHC.HsVar GHC.noExt bindL
#elif __GLASGOW_HASKELL__ >= 800
  bindL <- locate (GHC.Unqual (GHC.mkDataOcc ">>="))
  liftT $ addSimpleAnnT bindL (DP (0,1)) [((G GHC.AnnVal),DP (0,0))]
  bind_occ <- locate $ GHC.HsVar bindL
#else
  bind_occ <- locate $ GHC.HsVar (GHC.Unqual (GHC.mkDataOcc ">>="))
#endif
  let occDp = [(G GHC.AnnVal, DP (0,1))]
      occAnn = annNone {annsDP = occDp}
  liftT $ addAnn bind_occ occAnn
#if __GLASGOW_HASKELL__ >= 806
  nameL <- locate name
  liftT $ addSimpleAnnT nameL (DP (0,1)) [((G GHC.AnnVal),DP (0,0))]
  l_name <- locate $ GHC.HsVar GHC.noExt nameL
#elif __GLASGOW_HASKELL__ >= 800
  nameL <- locate name
  liftT $ addSimpleAnnT nameL (DP (0,1)) [((G GHC.AnnVal),DP (0,0))]
  l_name <- locate $ GHC.HsVar nameL
#else
  l_name <- locate $ GHC.HsVar name
#endif
  let l_ann = annNone {annsDP = [(G GHC.AnnVal, DP (0,1))]}
  liftT $ addAnn l_name l_ann
#if __GLASGOW_HASKELL__ >= 806
  oppApp <- locate $ GHC.OpApp GHC.noExt l_name bind_occ lam_par
#else
  oppApp <- locate $ GHC.OpApp l_name bind_occ GHC.PlaceHolder lam_par
#endif
  addEmptyAnn oppApp
#if __GLASGOW_HASKELL__ >= 806
  lgrhs <- locate $ GHC.GRHS GHC.noExt [] oppApp
#else
  lgrhs <- locate $ GHC.GRHS [] oppApp
#endif
  addEmptyAnn lgrhs
#if __GLASGOW_HASKELL__ >= 806
  return $ GHC.GRHSs GHC.noExt [lgrhs] (GHC.noLoc (GHC.EmptyLocalBinds GHC.noExt ))
#elif __GLASGOW_HASKELL__ >= 800
  return $ GHC.GRHSs [lgrhs] (GHC.noLoc GHC.EmptyLocalBinds)
#else
  return $ GHC.GRHSs [lgrhs] GHC.EmptyLocalBinds
#endif

-- | This takes an AST chunk traverses it and changes any calls to the "Just"
-- constructor to "return"
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
getVarAndRHS :: GHC.Match GhcPs (GHC.LHsExpr GhcPs) -> RefactGhc (GHC.LPat GhcPs, ParsedGRHSs)
getVarAndRHS match = do
  let (Just pat) = SYB.something (Nothing `SYB.mkQ` varPat) (GHC.m_pats match)
  return (pat , GHC.m_grhss match)
    where varPat lPat@(GHC.L _ (GHC.VarPat _ )) = Just lPat
          varPat _ = Nothing


--Looks up the function binding at the given position. Returns nothing if the position does not contain a binding.
getHsBind :: (Data a) => SimpPos -> String -> a -> Maybe (GHC.HsBind GhcPs)
getHsBind pos funNm a =
  let rdrNm = locToRdrName pos a in
  case rdrNm of
  Nothing -> Nothing
  (Just (GHC.L _ rNm)) -> SYB.everythingStaged SYB.Parser (<|>) Nothing (Nothing `SYB.mkQ` isBind) a
    where isBind (bnd@(GHC.FunBind (GHC.L _ name) _ _ _ _ _) :: GHC.HsBind GhcPs)
            | name == rNm = (Just bnd)
          isBind _ = Nothing
-}

-- ---------------------------------------------------------------------

-- | This function takes in the name of a function and determines if the binding
-- contains the case "Nothing = Nothing" If the Nothing to Nothing case is found
-- then it is removed from the parsed source.
containsNothingToNothing :: Int -> SimpPos -> GHC.HsBind GhcPs
                         -> RefactGhc (Maybe (GHC.HsBind GhcPs))
containsNothingToNothing argNum pos bind = do
  let
      -- bind = gfromJust "containsNothingToNothing" $ getHsBind pos parsed
      mg = GHC.fun_matches bind
  let oldMatches = SYB.everything (++) ([] `SYB.mkQ` isNtoNMatch) bind
  if null oldMatches
    then return Nothing
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
    removeMatches pos newBind oldMatches -- AZ, remove this some time
    return (Just newBind)
  where
    isNtoNMatch :: ParsedLMatch -> [ParsedLMatch]
    isNtoNMatch lm@(GHC.L _ m) =
      let rhsCheck = checkGRHS $ GHC.m_grhss m
          lhsCheck = checkPats $ GHC.m_pats m in
      if (rhsCheck && lhsCheck)
        then [lm]
        else []

    checkGRHS :: ParsedGRHSs -> Bool
#if __GLASGOW_HASKELL__ >= 806
    checkGRHS (GHC.GRHSs _ [(GHC.L _ (GHC.GRHS _ _ (GHC.L _ body)))] _)  = isNothingVar body
#else
    checkGRHS (GHC.GRHSs [(GHC.L _ (GHC.GRHS _ (GHC.L _ body)))] _)  = isNothingVar body
#endif
    checkGRHS _ = False

    checkPats :: [GHC.LPat GhcPs] -> Bool
    checkPats patLst =
      if argNum <= length patLst
      then let (LL _ pat) = patLst !! (argNum - 1) in
      isNothingPat pat
      else False

    filterMatch :: ParsedLMatch -> [ParsedLMatch] -> [ParsedLMatch]
    filterMatch (GHC.L l1 _) = filter (\(GHC.L l2 _) -> l1 /= l2)

    getNewMs :: [ParsedLMatch] -> [ParsedLMatch] -> [ParsedLMatch]
    getNewMs [] lst = lst
    getNewMs (m:ms) lst = let newLst = filterMatch m lst in
      getNewMs ms newLst

    isNothingPat :: GHC.Pat GhcPs -> Bool
#if __GLASGOW_HASKELL__ >= 806
    isNothingPat (GHC.VarPat _ (GHC.L _ nm)) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#elif __GLASGOW_HASKELL__ >= 800
    isNothingPat (GHC.VarPat (GHC.L _ nm)) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#else
    isNothingPat (GHC.VarPat nm) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#endif
    isNothingPat (GHC.ConPatIn (GHC.L _ nm) _) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
    isNothingPat _ = False
    isNothingVar :: GHC.HsExpr GhcPs -> Bool
#if __GLASGOW_HASKELL__ >= 806
    isNothingVar (GHC.HsVar _ (GHC.L _ nm)) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#elif __GLASGOW_HASKELL__ >= 800
    isNothingVar (GHC.HsVar (GHC.L _ nm)) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#else
    isNothingVar (GHC.HsVar nm) = ((GHC.occNameString . GHC.rdrNameOcc) nm) == "Nothing"
#endif
    isNothingVar _ = False

-- Removes the given matches from the given binding. Uses the position to retrieve the rdrName.
removeMatches :: SimpPos -> GHC.HsBind GhcPs -> [GHC.LMatch GhcPs (GHC.LHsExpr GhcPs)]
              -> RefactGhc ()
removeMatches pos newBind matches = do
  parsed <- getRefactParsed
  let rdrNm = gfromJust "Couldn't get rdrName in removeMatch" $ locToRdrName pos parsed
  newParsed <- SYB.everywhereM (SYB.mkM (replaceBind' rdrNm)) parsed
  mapM_ removeAnns matches
  putRefactParsed newParsed mempty
  return ()
    where replaceBind' :: GHC.Located GHC.RdrName -> GHC.HsBind GhcPs -> RefactGhc (GHC.HsBind GhcPs)
          replaceBind' rdrNm ((GHC.FunBind { GHC.fun_id = name }) :: GHC.HsBind GhcPs)
            | name == rdrNm = return newBind
          replaceBind' _ a = return a

-- | This function is very specific to Maybe to MonadPlus refactoring. It rewrites
--the type signature so that the calls to maybe will be replaced with type
--variable "m" and adds the MonadPlus type class constraint to m
--Assumptions:
--  Assumes the function is of type Maybe a -> Maybe a
--
--   Should refactor to take in the argNum parameter and fix the type depending on that
fixType' :: GHC.Name -> Int -> RefactGhc ()
fixType' funNm argPos = do
  nm <- getRefactNameMap
  logm "fixType':Fixing type"
  parsed <- getRefactParsed
  -- TODO:AZ what if there is no signature?
  let m_sig                       = getSigD nm funNm parsed
#if __GLASGOW_HASKELL__ >= 806
      (GHC.L sigL (GHC.SigD x sig)) = gfromJust "fixType'" m_sig
#else
      (GHC.L sigL (GHC.SigD sig)) = gfromJust "fixType'" m_sig
#endif
  fixedClass <- fixTypeClass sig
  --This needs to be fixed to replace only the correct argument and output type
  replacedMaybe <- replaceMaybeWithVariable fixedClass

#if __GLASGOW_HASKELL__ >= 806
  newSig <- locate (GHC.SigD x replacedMaybe)
#else
  newSig <- locate (GHC.SigD replacedMaybe)
#endif
  _ <- synthesizeAnns newSig
  addNewLines 2 newSig
  addNewKeyword ((G GHC.AnnDcolon), DP (0,1)) newSig

  logm $ "fixType':Span: " ++ show sigL
  newParsed <- replaceAtLocation sigL newSig
  -- logDataWithAnns "fixType':newParsed" newParsed
  putRefactParsed newParsed mempty
    where replaceMaybeWithVariable :: GHC.Sig GhcPs -> RefactGhc (GHC.Sig GhcPs)
          replaceMaybeWithVariable sig = SYB.everywhereM (SYB.mkM worker) sig
            where
                  worker :: GHC.HsType GhcPs -> RefactGhc (GHC.HsType GhcPs)
#if __GLASGOW_HASKELL__ >= 806
                  worker tyVar@(GHC.HsTyVar x p (GHC.L lv rdrName))
                    | compNames "Maybe" rdrName = let newRdr = (GHC.mkVarUnqual . GHC.fsLit) "m" in
                        return (GHC.HsTyVar x p (GHC.L lv newRdr))
                    | otherwise = return tyVar
#elif __GLASGOW_HASKELL__ >= 802
                  worker tyVar@(GHC.HsTyVar p (GHC.L lv rdrName))
                    | compNames "Maybe" rdrName = let newRdr = (GHC.mkVarUnqual . GHC.fsLit) "m" in
                        return (GHC.HsTyVar p (GHC.L lv newRdr))
                    | otherwise = return tyVar
#elif __GLASGOW_HASKELL__ >= 800
                  worker tyVar@(GHC.HsTyVar (GHC.L lv rdrName))
                    | compNames "Maybe" rdrName = let newRdr = (GHC.mkVarUnqual . GHC.fsLit) "m" in
                        return (GHC.HsTyVar (GHC.L lv newRdr))
                    | otherwise = return tyVar
#else
                  worker tyVar@(GHC.HsTyVar rdrName)
                    | compNames "Maybe" rdrName = let newRdr = (GHC.mkVarUnqual . GHC.fsLit) "m" in
                        return (GHC.HsTyVar newRdr)
                    | otherwise = return tyVar
#endif
                  worker var = return var
          fixTypeClass :: GHC.Sig GhcPs -> RefactGhc (GHC.Sig GhcPs)
#if __GLASGOW_HASKELL__ >= 806
          fixTypeClass (GHC.TypeSig x names (GHC.HsWC wcs (GHC.HsIB a (GHC.L lt hsType)))) =
            case hsType of
              (GHC.HsQualTy _ context ty) -> do
                newContext <- case context of
                                (GHC.L _ []) -> do
                                  tyCls <- genMonadPlusClass
                                  parTy <- locate (GHC.HsParTy GHC.noExt tyCls)
                                  addNewKeyword ((G GHC.AnnCloseP),DP (0,0)) parTy
                                  liftT $ setPrecedingLinesT parTy 0 (-1)
                                  lList <- locate [parTy]
                                  addNewKeywords [((G GHC.AnnOpenP), DP (0,0)),((G GHC.AnnCloseP), DP (0,-1)),((G GHC.AnnDarrow), DP (0,1))] lList
                                  return lList
                                (GHC.L _ [(GHC.L _ (GHC.HsParTy _ innerTy))]) -> do
                                  tyCls <- genMonadPlusClass
                                  lList <- locate [innerTy,tyCls]
                                  return lList
                                (GHC.L _ lst) -> do
                                  tyCls <- genMonadPlusClass
                                  lList <- locate (tyCls:lst)
                                  return lList
                newForAll <- locate (GHC.HsQualTy GHC.noExt newContext ty)
                liftT $ setPrecedingLinesT ty 0 1
                liftT $ setPrecedingLinesT newForAll 0 1
                return (GHC.TypeSig x names (GHC.HsWC wcs (GHC.HsIB a newForAll)))
              (GHC.HsForAllTy _ _bndrs ty) -> do
                newContext <- do
                                  tyCls <- genMonadPlusClass
                                  parTy <- locate (GHC.HsParTy GHC.noExt tyCls)
                                  addNewKeyword ((G GHC.AnnCloseP),DP (0,0)) parTy
                                  liftT $ setPrecedingLinesT parTy 0 (-1)
                                  lList <- locate [parTy]
                                  addNewKeywords [((G GHC.AnnOpenP), DP (0,0)),((G GHC.AnnCloseP), DP (0,-1)),((G GHC.AnnDarrow), DP (0,1))] lList
                                  return lList
                -- newForAll <- locate (GHC.HsForAllTy b ty)
                newForAll <- locate (GHC.HsQualTy GHC.noExt newContext ty)
                liftT $ setPrecedingLinesT ty 0 1
                liftT $ setPrecedingLinesT newForAll 0 1
                -- return (GHC.TypeSig names newForAll p)
                return (GHC.TypeSig x names (GHC.HsWC wcs (GHC.HsIB a newForAll)))
              _unexpected -> do
                logm "fixTypeClass:unexpected"
                -- logDataWithAnns "fixTypeClass:unexpected" unexpected
                newContext <- do
                  tyCls <- genMonadPlusClass
                  parTy <- locate (GHC.HsParTy GHC.noExt tyCls)
                  liftT $ setPrecedingLinesT parTy 0 (-1)
                  lList <- locate [parTy]
                  addNewKeywords [((G GHC.AnnOpenP), DP (0,1)),((G GHC.AnnCloseP), DP (0,0)),((G GHC.AnnDarrow), DP (0,1))] lList
                  return lList
                let qualTy = GHC.HsQualTy GHC.noExt newContext (GHC.L lt hsType)
                qualTyL <- locate qualTy
                return (GHC.TypeSig x names (GHC.HsWC wcs (GHC.HsIB a qualTyL)))
#elif __GLASGOW_HASKELL__ >= 802
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
                logm "fixTypeClass:unexpected"
                -- logDataWithAnns "fixTypeClass:unexpected" unexpected
                newContext <- do
                  tyCls <- genMonadPlusClass
                  parTy <- locate (GHC.HsParTy tyCls)
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

          genMonadPlusClass :: RefactGhc (GHC.LHsType GhcPs)
          genMonadPlusClass = do
            -- let mPlusNm = GHC.mkVarUnqual (GHC.fsLit "MonadPlus")
            --     mNm     = GHC.mkVarUnqual (GHC.fsLit "m")
            let mPlusNm = mkRdrName "MonadPlus"
                mNm     = mkRdrName "m"
#if __GLASGOW_HASKELL__ >= 806
            mPlusNmL <- locate mPlusNm
            addAnnValWithDP mPlusNmL (DP (0,0))
            lPlus <- locate (GHC.HsTyVar GHC.noExt GHC.NotPromoted mPlusNmL)
#elif __GLASGOW_HASKELL__ >= 802
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
#if __GLASGOW_HASKELL__ >= 806
            mNmL <- locate mNm
            addAnnVal mNmL
            lM <- locate (GHC.HsTyVar GHC.noExt GHC.NotPromoted mNmL)
            lApp <- locate (GHC.HsAppTy GHC.noExt lPlus lM)
#elif __GLASGOW_HASKELL__ >= 802
            mNmL <- locate mNm
            addAnnVal mNmL
            lM <- locate (GHC.HsTyVar GHC.NotPromoted mNmL)
            lApp <- locate (GHC.HsAppTy lPlus lM)
#elif __GLASGOW_HASKELL__ >= 800
            mNmL <- locate mNm
            addAnnVal mNmL
            lM <- locate (GHC.HsTyVar mNmL)
            lApp <- locate (GHC.HsAppTy lPlus lM)
#else
            lM <- locate (GHC.HsTyVar mNm)
            addAnnVal lM
            lApp <- locate (GHC.HsAppTy lPlus lM)
#endif
            return lApp


fixType :: GHC.Name -> RefactGhc ()
fixType funNm = do
  nm <- getRefactNameMap
  parsed <- getRefactParsed
  currAnns <- fetchAnnsFinal
  dFlags <- GHC.getSessionDynFlags
  logm $ "fixType:funNm=" ++ showGhc funNm
  -- logParsedSource "fixType"
  let m_sig = getSigD nm funNm parsed
#if __GLASGOW_HASKELL__ >= 806
      (GHC.L sigL (GHC.SigD _ sig)) = gfromJust "fixType: getting sig" m_sig
#else
      (GHC.L sigL (GHC.SigD sig)) = gfromJust "fixType: getting sig" m_sig
#endif
      iType = gfromJust "fixType: iType" $ getInnerType sig
      strTy = exactPrint iType currAnns
#if __GLASGOW_HASKELL__ >= 806
      tyStr = showGhc funNm ++ " :: (MonadPlus m) => m" ++ strTy ++ " -> m" ++ strTy
#else
      tyStr = showGhc funNm ++ " :: (MonadPlus m) => m " ++ strTy ++ " -> m " ++ strTy
#endif
      pRes = parseDecl dFlags "MaybeToMonadPlus.hs" tyStr
  logm $ "fixType:sig " ++ showGhc sig
  logm $ "fixType:iType " ++ showGhc iType
  logm $ "Type string:strTy " ++ strTy
  logm $ "Type string:tyStr " ++ tyStr
  -- logDataWithAnns "fixType:sig" sig
  (anns, newSig) <- handleParseResult "MaybeToMonadPlus.fixType" pRes
  newParsed <- replaceAtLocation sigL newSig
  putRefactParsed newParsed anns
  addNewLines 2 newSig

getSigD :: (Data a) => NameMap -> GHC.Name -> a -> Maybe (GHC.LHsDecl GhcPs)
getSigD nm funNm = SYB.something (Nothing `SYB.mkQ` isSigD)
  where
    isSigD :: GHC.LHsDecl GhcPs -> Maybe (GHC.LHsDecl GhcPs)
#if __GLASGOW_HASKELL__ >= 806
    isSigD s@(GHC.L _ (GHC.SigD _ sig)) = if isSig sig
#else
    isSigD s@(GHC.L _ (GHC.SigD sig)) = if isSig sig
#endif
                                          then Just s
                                          else Nothing
    isSigD _ = Nothing

    isSig :: GHC.Sig GhcPs -> Bool
    -- TODO:AZ: what about a shared signature, where the one we care about is not the first one?
#if __GLASGOW_HASKELL__ >= 806
    isSig (GHC.TypeSig _ [ln] _) = (sameName nm ln funNm)
#elif __GLASGOW_HASKELL__ >= 800
    isSig sig@(GHC.TypeSig [ln] _) = (sameName nm ln funNm)
#else
    isSig sig@(GHC.TypeSig [ln] _ _) = (sameNAme nm ln funNm)
#endif
    isSig _ = False

-- TODO:AZ we should be using exact stuff for this, matching things like "Just"
compNames :: String -> GHC.RdrName -> Bool
compNames s rdr = let sRdr = (GHC.occNameString . GHC.rdrNameOcc) rdr in
  sRdr == s


-- ---------------------------------------------------------------------

-- TODO:AZ this should make use of argNum, to make sure it only processes the
--      param to be refactored.
-- | Get the type inside the first Maybe the traversal finds.
getInnerType :: GHC.Sig GhcPs -> Maybe (GHC.LHsType GhcPs)
getInnerType = SYB.everything (<|>) (Nothing `SYB.mkQ` getTy)
#if __GLASGOW_HASKELL__ >= 806
  where getTy :: GHC.HsType GhcPs -> Maybe (GHC.LHsType GhcPs)
        getTy (GHC.HsAppTy _ mCon  otherTy)
          = if isMaybeTy mCon
              then Just otherTy
              else Nothing
        getTy _ = Nothing

        isMaybeTy :: GHC.LHsType GhcPs -> Bool
        isMaybeTy (GHC.L _ (GHC.HsTyVar _ _ (GHC.L _ (GHC.Unqual occNm)))) = (GHC.occNameString occNm) == "Maybe"
        isMaybeTy _ = False
#elif __GLASGOW_HASKELL__ >= 802
  where getTy :: GHC.HsType GhcPs -> Maybe (GHC.LHsType GhcPs)
        getTy (GHC.HsAppsTy [GHC.L _ (GHC.HsAppPrefix mCon), GHC.L _ (GHC.HsAppPrefix otherTy)])
          = if isMaybeTy mCon
              then Just otherTy
              else Nothing
        getTy _ = Nothing

        isMaybeTy :: GHC.LHsType GhcPs -> Bool
        isMaybeTy (GHC.L _ (GHC.HsTyVar _ (GHC.L _ (GHC.Unqual occNm)))) = (GHC.occNameString occNm) == "Maybe"
        isMaybeTy _ = False
#elif __GLASGOW_HASKELL__ >= 800
  where getTy :: GHC.HsType GhcPs -> Maybe (GHC.LHsType GhcPs)
        getTy (GHC.HsAppsTy [GHC.L _ (GHC.HsAppPrefix mCon), GHC.L _ (GHC.HsAppPrefix otherTy)])
          = if isMaybeTy mCon
                                             then Just otherTy
                                             else Nothing
        getTy _ = Nothing

        isMaybeTy :: GHC.LHsType GhcPs -> Bool
        isMaybeTy (GHC.L _ (GHC.HsTyVar (GHC.L _ (GHC.Unqual occNm)))) = (GHC.occNameString occNm) == "Maybe"
        isMaybeTy _ = False
#else
  where getTy :: GHC.HsType GhcPs -> Maybe (GHC.LHsType GhcPs)
        getTy (GHC.HsAppTy mCon otherTy) = if isMaybeTy mCon
                                             then Just otherTy
                                             else Nothing
        getTy _ = Nothing

        isMaybeTy :: GHC.LHsType GhcPs -> Bool
        isMaybeTy (GHC.L _ (GHC.HsTyVar (GHC.Unqual occNm))) = (GHC.occNameString occNm) == "Maybe"
        isMaybeTy _ = False
#endif

-- ---------------------------------------------------------------------

replaceAtLocation :: (Data a) => GHC.SrcSpan -> GHC.Located a -> RefactGhc (GHC.ParsedSource)
replaceAtLocation ss new = do
  logm $ "replaceAtLocation:Span: " ++ (show ss)
  parsed <- getRefactParsed
  newParsed <- SYB.everywhereM (SYB.mkM findLoc) parsed
  return newParsed
    where --findLoc :: (forall b. (Data b) => GHC.Located b -> RefactGhc (GHC.Located b))
          findLoc a@(GHC.L l s) = if l == ss
                                  then do
                                    _ <- removeAnns s
                                    return new
                                  else return a
