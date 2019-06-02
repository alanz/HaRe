{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Refact.Utils.Transform
  (
    addSimpleImportDecl
  , wrapInLambda
  , wrapInPars
  , wrapInParsWithDPs
  , addNewLines
  , locate
  , locWithAnnVal
  , removePars
  , replaceTypeSig
  , replaceFunBind
  , addBackquotes
  , constructLHsTy
  , constructHsVar
  , insertNewDecl
  , rmFun
  , replaceFunRhs
  , traverseTypeSig
  ) where

import qualified GHC as GHC
import qualified BasicTypes as GHC
import qualified Data.Map as Map
import Data.Data
import Data.Maybe
import qualified Data.Generics as SYB
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.Refact.Utils.Monad
import Language.Haskell.Refact.Utils.MonadFunctions
import Language.Haskell.Refact.Utils.Types
import Language.Haskell.Refact.Utils.TypeUtils
import Language.Haskell.Refact.Utils.Synonyms
import Language.Haskell.Refact.Utils.ExactPrint


-- The goal of this module is to provide basic transformations of the ast and
-- annotations that are useful in multiple refactorings


--Takes in a string corresponding to the module name to be imported
--Adds the import declaration at the end of that module's imports
addSimpleImportDecl :: String -> Maybe String -> RefactGhc ()
addSimpleImportDecl modName mqual = do
  let modNm' = GHC.mkModuleName modName
  parsed <- getRefactParsed
  newP <- addImportDecl parsed modNm' Nothing False False (isJust mqual) mqual False []
  putRefactParsed newP mempty



-- | Locates an piece of abstract syntax and adds a AnnVal annotation
-- at the new location
#if __GLASGOW_HASKELL__ >= 808
locWithAnnVal :: Constraints a => GHC.SrcSpanLess a -> RefactGhc a
#else
locWithAnnVal :: Data a => a -> RefactGhc (GHC.Located a)
#endif
locWithAnnVal a = do
  lA <- locate a
  addAnnVal lA
  return lA


-- |Takes in a lhs pattern and a rhs. Wraps those in a lambda and adds the
--annotations associated with the lambda. Returns the new located lambda
--expression
wrapInLambda :: GHC.LPat GhcPs -> ParsedGRHSs -> RefactGhc (GHC.LHsExpr GhcPs)
wrapInLambda varPat rhs = do
  match@(GHC.L l _match)  <- mkLamMatch varPat rhs
  --logm $ "Match: " ++ (SYB.showData SYB.Parser 3 match)
#if __GLASGOW_HASKELL__ >= 806
  lMatchLst <- locate [match]
  let mg = GHC.MG GHC.noExt lMatchLst GHC.Generated
#elif __GLASGOW_HASKELL__ > 710
  lMatchLst <- locate [match]
  let mg = GHC.MG lMatchLst [] GHC.PlaceHolder GHC.Generated
#else
  let mg = GHC.MG [match] [] GHC.PlaceHolder GHC.Generated
  currAnns <- fetchAnnsFinal
#endif
  -- currAnns <- fetchAnnsFinal
  --logm $ "Anns :" ++ (show $ getAllAnns currAnns match)
#if __GLASGOW_HASKELL__ >= 806
  let l_lam = GHC.L l (GHC.HsLam GHC.noExt mg)
#else
  let l_lam = GHC.L l (GHC.HsLam mg)
#endif
  par_lam <- wrapInPars l_lam
  -- logDataWithAnns "newLambda" par_lam
  -- logExactprint "lam2" l_lam
  return par_lam

-- |This function makes a match suitable for use inside of a lambda expression.
mkLamMatch :: GHC.LPat GhcPs -> GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs)
           -> RefactGhc (GHC.LMatch GhcPs (GHC.LHsExpr GhcPs))
mkLamMatch varPat rhs = do
#if __GLASGOW_HASKELL__ >= 806
  lMatch <- locate (GHC.Match GHC.noExt GHC.LambdaExpr [varPat]         rhs)
#elif __GLASGOW_HASKELL__ >= 804
  lMatch <- locate (GHC.Match           GHC.LambdaExpr [varPat]         rhs)
#elif __GLASGOW_HASKELL__ > 800
  lMatch <- locate (GHC.Match           GHC.LambdaExpr [varPat] Nothing rhs)
#elif __GLASGOW_HASKELL__ > 710
  lMatch <- locate (GHC.Match GHC.NonFunBindMatch [varPat] Nothing rhs)
#else
  lMatch <- locate (GHC.Match Nothing [varPat] Nothing rhs)
#endif
  let dp = [(G GHC.AnnRarrow, DP (0,1)),(G GHC.AnnLam, DP (0,0))]
      newAnn = annNone {annsDP = dp, annEntryDelta = DP (0,0)}
  zeroDP varPat
  liftT $ addAnn lMatch newAnn
  return lMatch

wrapInParsWithDPs :: DeltaPos -> DeltaPos -> GHC.LHsExpr GhcPs -> RefactGhc (GHC.LHsExpr GhcPs)
wrapInParsWithDPs openDP closeDP expr = do
#if __GLASGOW_HASKELL__ >= 806
  newAst <- locate (GHC.HsPar GHC.noExt expr)
#else
  newAst <- locate (GHC.HsPar expr)
#endif
  let dp = [(G GHC.AnnOpenP, openDP), (G GHC.AnnCloseP, closeDP)]
      newAnn = annNone {annsDP = dp}
  liftT $ addAnn newAst newAnn
  return newAst


--Wraps a given expression in parenthesis and add the appropriate annotations, returns the modified ast chunk.
wrapInPars :: GHC.LHsExpr GhcPs -> RefactGhc (GHC.LHsExpr GhcPs)
wrapInPars = wrapInParsWithDPs (DP (0,1)) (DP (0,0))

--Does the opposite of wrapInPars
removePars :: ParsedLExpr -> RefactGhc ParsedLExpr
#if __GLASGOW_HASKELL__ >= 806
removePars (GHC.L _ (GHC.HsPar _ expr)) = do
#else
removePars (GHC.L _ (GHC.HsPar expr)) = do
#endif
  setDP (DP (0,1)) expr
  return expr
removePars expr = return expr

--Takes a piece of AST and adds an n row offset
addNewLines :: (Data a) => Int -> GHC.Located a -> RefactGhc ()
addNewLines n ast = do
  currAnns <- fetchAnnsFinal
  let key = mkAnnKey ast
      mv = Map.lookup key currAnns
  case mv of
    Nothing -> return ()
    Just v -> do
      let (DP (row,col)) = annEntryDelta v
          newDP = (DP (row+n,col))
          newAnn = v {annEntryDelta = newDP}
          newAnns = Map.insert key newAnn currAnns
      setRefactAnns newAnns


--This function replaces the type signature of the function that is defined at the simple position
replaceTypeSig :: SimpPos -> GHC.Sig GhcPs -> RefactGhc ()
replaceTypeSig pos sig = do
  parsed <- getRefactParsed
  let (Just (GHC.L _ rdrNm)) = locToRdrName pos parsed
  newParsed <- SYB.everywhereM (SYB.mkM (comp rdrNm)) parsed
  putRefactParsed newParsed emptyAnns
    where comp :: GHC.RdrName -> GHC.Sig GhcPs -> RefactGhc (GHC.Sig GhcPs)
#if __GLASGOW_HASKELL__ >= 806
          comp nm oldTy@(GHC.TypeSig _ [(GHC.L _ oldNm)]  _)
#elif __GLASGOW_HASKELL__ > 710
          comp nm oldTy@(GHC.TypeSig [(GHC.L _ oldNm)]  _)
#else
          comp nm oldTy@(GHC.TypeSig [(GHC.L _ oldNm)]  _ _)
#endif
            | oldNm == nm = return sig
            | otherwise = return oldTy
          comp _ x = return x

--Replaces the HsBind at the given position
replaceFunBind :: SimpPos -> ParsedBind -> RefactGhc ()
replaceFunBind pos bnd = do
  parsed <- getRefactParsed
  let (Just (GHC.L _ rdrNm)) = locToRdrName pos parsed
  newParsed <- SYB.everywhereM (SYB.mkM (comp rdrNm)) parsed
  putRefactParsed newParsed emptyAnns
    where comp :: GHC.RdrName -> GHC.HsBind GhcPs -> RefactGhc (GHC.HsBind GhcPs)
          comp nm b@(GHC.FunBind { GHC.fun_id = (GHC.L _ n) })
            | n == nm = return bnd
            | otherwise = return b
          comp _ x = return x


--Adds backquotes around a function call
addBackquotes :: ParsedLExpr -> RefactGhc ()
addBackquotes var = do
  anns <- liftT getAnnsT
  let (Just oldAnn) = Map.lookup (mkAnnKey var) anns
      annsLst = annsDP oldAnn
      bqtAnn = ((G GHC.AnnBackquote),DP (0,0))
      newLst = bqtAnn:(annsLst++[bqtAnn])
      newAnn = oldAnn {annsDP = newLst}
  liftT $ addAnn var newAnn


-- | The next two functions construct variables and type variables
--respectively from RdrNames.  They keep the amount of CPP in
--refactoring code to a minimum.
constructHsVar :: GHC.RdrName -> RefactGhc (GHC.LHsExpr GHC.GhcPs)
constructHsVar nm = do
#if __GLASGOW_HASKELL__ >= 806
  logm $ "New var construction ghc 8"
  lNm <- locate nm
  addAnnVal lNm
  zeroDP lNm
  newVar <- locate (GHC.HsVar GHC.noExt lNm) :: RefactGhc (GHC.LHsExpr GHC.GhcPs)
#elif __GLASGOW_HASKELL__ > 710
  logm $ "New var construction ghc 8"
  lNm <- locate nm
  addAnnVal lNm
  zeroDP lNm
  newVar <- locate (GHC.HsVar lNm)
#else
  newVar <- locate (GHC.HsVar nm)
#endif
  addAnnVal newVar
  return newVar

constructLHsTy :: GHC.RdrName -> RefactGhc (GHC.LHsType GhcPs)
constructLHsTy nm = do
#if __GLASGOW_HASKELL__ >= 806
  lNm <- locate nm
  newTy <- locate (GHC.HsTyVar GHC.noExt GHC.NotPromoted lNm)
  addAnnVal lNm
  zeroDP lNm
#elif __GLASGOW_HASKELL__ >= 802
  lNm <- locate nm
  newTy <- locate (GHC.HsTyVar GHC.NotPromoted lNm)
  addAnnVal lNm
  zeroDP lNm
#elif __GLASGOW_HASKELL__ >= 800
  lNm <- locate nm
  newTy <- locate (GHC.HsTyVar lNm)
  lNm <- locate nm
  addAnnVal lNm
  zeroDP lNm
  newTy <- locate (GHC.HsTyVar lNm)
#else
  logm "New ty var ghc 7"
  newTy <- locate (GHC.HsTyVar nm)
#endif
  addAnnVal newTy
  zeroDP newTy
  return newTy

-- Inserts the string representation of the decl
-- at the end of the target module returns the location
-- of the new declaration
insertNewDecl :: String -> RefactGhc ParsedLDecl
insertNewDecl declStr = do
  (GHC.L pSpn hsMod) <- getRefactParsed
  -- trgtMod <- getRefactTargetModule
  -- Use a fake filepath the ensure that the location remains unique
  let fp = "HaRe"--GM.mpPath trgtMod
  logm $ "Inserting decl into " ++ fp
  df <- GHC.getSessionDynFlags
  let pRes = parseDecl df fp declStr
  case pRes of
    Left (_spn, str) -> error $ "insertNewDecl: decl parse failed with message:\n" ++ str
    Right (anns, decl) -> do
      let oldDecs = GHC.hsmodDecls hsMod
          newParsed = (GHC.L pSpn (hsMod {GHC.hsmodDecls = oldDecs ++ [decl]}))
      putRefactParsed newParsed anns
      addNewLines 1 decl
      return decl

rmFun :: GHC.RdrName -> RefactGhc ()
rmFun nm = do
  parsed <- getRefactParsed
  let newP = SYB.everywhere (SYB.mkT filterDeclLst) parsed
  putRefactParsed newP mempty
    where filterDeclLst :: [GHC.LHsDecl GhcPs] -> [GHC.LHsDecl GhcPs]
          filterDeclLst = filter (\dec -> not $ isFun dec)
#if __GLASGOW_HASKELL__ >= 806
          isFun (GHC.L _ (GHC.ValD _ (GHC.FunBind _ lNm _ _ _))) = (GHC.unLoc lNm) == nm
#elif __GLASGOW_HASKELL__ > 710
          isFun (GHC.L _ (GHC.ValD (GHC.FunBind lNm _ _ _ _))) = (GHC.unLoc lNm) == nm
#else
          isFun (GHC.L _ (GHC.ValD (GHC.FunBind lNm _ _ _ _ _))) = (GHC.unLoc lNm) == nm
#endif
          isFun _ = False


replaceFunRhs :: SimpPos -> GHC.LHsExpr GhcPs -> RefactGhc ()
replaceFunRhs pos newRhs = do
  parsed <- getRefactParsed
  let rdrNm = locToRdrName pos parsed
  case rdrNm of
    Nothing -> error "replaceFunRhs: Position does not correspond to a binding."
    (Just (GHC.L _ rNm)) -> do
      newParsed <- SYB.everywhereM (SYB.mkM (worker rNm)) parsed
      putRefactParsed newParsed emptyAnns
      logParsedSource "GenApplicative.replaceFunRhs"
  where worker :: GHC.RdrName -> ParsedBind -> RefactGhc ParsedBind
#if __GLASGOW_HASKELL__ >= 806
        worker rNm fBind@(GHC.FunBind _ (GHC.L _ fNm) mg _ _)
#elif __GLASGOW_HASKELL__ > 710
        worker rNm fBind@(GHC.FunBind (GHC.L _ fNm) mg _ _ _)
#else
        worker rNm fBind@(GHC.FunBind (GHC.L _ fNm) _ mg _ _ _)
#endif
          | fNm == rNm = do
              newMg <- replaceMG mg
              return $ fBind{GHC.fun_matches = newMg}
          | otherwise = return fBind
        worker _ bind = return bind

        replaceMG :: GHC.MatchGroup GhcPs (GHC.LHsExpr GhcPs)
                  -> RefactGhc (GHC.MatchGroup GhcPs (GHC.LHsExpr GhcPs))
        replaceMG mg = do
#if __GLASGOW_HASKELL__ <= 710
          let [(GHC.L l match)] = GHC.mg_alts mg
#else
          let (GHC.L _ [(GHC.L l match)]) = GHC.mg_alts mg
#endif
              oldGrhss = GHC.m_grhss match
              newGrhss = mkGrhss oldGrhss newRhs
              newLMatch = (GHC.L l (match{GHC.m_grhss = newGrhss}))
#if __GLASGOW_HASKELL__ <= 710
          return mg{GHC.mg_alts = [newLMatch]}
#else
          lMatchLst <- locate [newLMatch]
          return mg{GHC.mg_alts = lMatchLst}
#endif
#if __GLASGOW_HASKELL__ >= 806
        mkGrhss old newExpr = let [(GHC.L l (GHC.GRHS x lst _))] = GHC.grhssGRHSs old in
          old { GHC.grhssGRHSs = [(GHC.L l (GHC.GRHS x lst newExpr))]}
#else
        mkGrhss old newExpr = let [(GHC.L l (GHC.GRHS lst _))] = GHC.grhssGRHSs old in
          old { GHC.grhssGRHSs = [(GHC.L l (GHC.GRHS lst newExpr))]}
#endif


--This function will apply the given function to the appropriate type signature element.
--The int denotes which argument the function should be applied to starting at one
--For example when: "traverseTypeSig 2 g"
--Is applied to the signature "f :: Int -> (Int -> String) -> String"
--g will be applied to "(Int -> String)"
--You also need to handle spacing before the type signature element
traverseTypeSig :: Int -> (GHC.LHsType GhcPs -> RefactGhc (GHC.LHsType GhcPs))
                -> GHC.Sig GhcPs -> RefactGhc (GHC.Sig GhcPs)
#if __GLASGOW_HASKELL__ >= 806
traverseTypeSig argNum f (GHC.TypeSig x lst (GHC.HsWC wcs (GHC.HsIB v ty'))) = do
  newTy <- comp argNum ty'
  return (GHC.TypeSig x lst (GHC.HsWC wcs (GHC.HsIB v newTy)))
  where
    comp argNum' (GHC.L l (GHC.HsForAllTy xx bndrs ty)) =
      case ty of
        (GHC.L _ (GHC.HsFunTy {})) -> comp' argNum' ty
          >>= (\res -> return (GHC.L l (GHC.HsForAllTy xx bndrs res)))
        _ -> f ty >>= (\res -> return (GHC.L l (GHC.HsForAllTy xx bndrs res)))
    comp _ ty = do
      logDataWithAnns "traverseTypeSig.comp:unknwon ty" ty
      error "foo"

    comp' 1 (GHC.L l (GHC.HsFunTy xx lhs rhs)) = do
      resLHS <- f lhs
      let funTy = (GHC.L l (GHC.HsFunTy xx resLHS rhs))
      zeroDP funTy
      return funTy
    comp' 1 lTy = f lTy
    comp' n (GHC.L l (GHC.HsFunTy xx lhs rhs)) = comp' (n-1) rhs
      >>= (\res -> return (GHC.L l (GHC.HsFunTy xx lhs res)))
    comp' _ lHsTy = return lHsTy
#elif __GLASGOW_HASKELL__ >= 802
traverseTypeSig argNum f (GHC.TypeSig lst (GHC.HsWC wcs (GHC.HsIB v ty' c))) = do
  newTy <- comp argNum ty'
  return (GHC.TypeSig lst (GHC.HsWC wcs (GHC.HsIB v newTy c)))
  where
    comp argNum' (GHC.L l (GHC.HsForAllTy bndrs ty)) =
      case ty of
        (GHC.L _ (GHC.HsFunTy _ _)) -> comp' argNum' ty >>= (\res -> return (GHC.L l (GHC.HsForAllTy bndrs res)))
        _ -> f ty >>= (\res -> return (GHC.L l (GHC.HsForAllTy bndrs res)))
    comp _ ty = do
      logDataWithAnns "traverseTypeSig.comp:unknwon ty" ty
      error "foo"

    comp' 1 (GHC.L l (GHC.HsFunTy lhs rhs)) = do
      resLHS <- f lhs
      let funTy = (GHC.L l (GHC.HsFunTy resLHS rhs))
      zeroDP funTy
      return funTy
    comp' 1 lTy = f lTy
    comp' n (GHC.L l (GHC.HsFunTy lhs rhs)) = comp' (n-1) rhs >>= (\res -> return (GHC.L l (GHC.HsFunTy lhs res)))
    comp' _ lHsTy = return lHsTy
#elif __GLASGOW_HASKELL__ >= 800
traverseTypeSig argNum f (GHC.TypeSig lst (GHC.HsIB vs (GHC.HsWC ns mc ty) )) = do
  newTy <- comp argNum ty
  return (GHC.TypeSig lst (GHC.HsIB vs (GHC.HsWC ns mc newTy) ))
  where
    comp :: Int -> GHC.LHsType GhcPs -> RefactGhc (GHC.LHsType GhcPs)
    comp argNum (GHC.L l (GHC.HsForAllTy bndrs ty)) =
      case ty of
        (GHC.L _ (GHC.HsFunTy _ _)) -> comp' argNum ty >>= (\res -> return (GHC.L l (GHC.HsForAllTy bndrs res)))
        _                           -> f ty            >>= (\res -> return (GHC.L l (GHC.HsForAllTy bndrs res)))

    comp' :: Int -> GHC.LHsType GhcPs -> RefactGhc (GHC.LHsType GhcPs)
    comp' 1 (GHC.L l (GHC.HsFunTy lhs rhs)) = do
      resLHS <- f lhs
      let funTy = (GHC.L l (GHC.HsFunTy resLHS rhs))
      zeroDP funTy
      return funTy
    comp' 1 lTy = f lTy
    comp' n (GHC.L l (GHC.HsFunTy lhs rhs)) = comp' (n-1) rhs >>= (\res -> return (GHC.L l (GHC.HsFunTy lhs res)))
    comp' _ lHsTy@(GHC.L _ ty) = return lHsTy
#else
traverseTypeSig argNum f (GHC.TypeSig lst ty rn) = do
  newTy <- comp argNum ty
  return (GHC.TypeSig lst newTy rn)
  where
    comp argNum (GHC.L l (GHC.HsForAllTy flg msp bndrs cntxt ty)) =
      case ty of
        (GHC.L _ (GHC.HsFunTy _ _)) -> comp' argNum ty >>= (\res -> return (GHC.L l (GHC.HsForAllTy flg msp bndrs cntxt res)))
        _ -> f ty >>= (\res -> return (GHC.L l (GHC.HsForAllTy flg msp bndrs cntxt res)))
    comp' 1 (GHC.L l (GHC.HsFunTy lhs rhs)) = do
      resLHS <- f lhs
      let funTy = (GHC.L l (GHC.HsFunTy resLHS rhs))
      zeroDP funTy
      return funTy
    comp' 1 lTy = f lTy
    comp' n (GHC.L l (GHC.HsFunTy lhs rhs)) = comp' (n-1) rhs >>= (\res -> return (GHC.L l (GHC.HsFunTy lhs res)))
    comp' _ lHsTy@(GHC.L _ ty) = return lHsTy
#endif

traverseTypeSig _ _ sig = error $ "traverseTypeSig: Unsupported constructor: " ++ show (toConstr sig)
