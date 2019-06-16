{-#LANGUAGE CPP #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Refact.Refactoring.Monadification
  (monadification,compMonadification) where

import Language.Haskell.Refact.API
import qualified Haskell.Ide.Engine.PluginApi as HIE (BiosOptions(..))
import Data.Generics as SYB
import qualified GHC as GHC
import Control.Monad.State
import Language.Haskell.GHC.ExactPrint.Types
import qualified Bag as GHC
#if __GLASGOW_HASKELL__ >= 808
import qualified BasicTypes as GHC (RecFlag,PromotionFlag(..))
#else
import qualified BasicTypes as GHC (RecFlag)
#endif

-- ---------------------------------------------------------------------

monadification :: RefactSettings -> HIE.BiosOptions -> FilePath -> [SimpPos] -> IO [FilePath]
monadification settings cradle fileName posLst = do
  absFileName <- normaliseFilePath fileName
  runRefacSession settings cradle (compMonadification absFileName posLst)


compMonadification :: FilePath -> [SimpPos] -> RefactGhc [ApplyRefacResult]
compMonadification fileName posLst = do
  (refRes@((_fp,ismod),_), ()) <- applyRefac (doMonadification posLst) (RSFile fileName)
  case ismod of
    RefacUnmodifed -> error "Monadification failed"
    RefacModified -> return ()
  return [refRes]

doMonadification :: [SimpPos] -> RefactGhc ()
doMonadification posLst = do
  nmsList <- getNames posLst
  mapM_ (monadifyFunc nmsList) posLst

getNames :: [SimpPos] -> RefactGhc [GHC.Name]
getNames posLst = mapM lookupNm posLst
  where lookupNm pos = do
          parsed <- getRefactParsed
          let rdr = gfromJust ("Unable to find name at " ++ (show pos)) (locToRdrName pos parsed)
          rdrName2Name rdr



monadifyFunc :: [GHC.Name] -> SimpPos -> RefactGhc ()
monadifyFunc nmsList pos = do
  parsed <- getRefactParsed
  -- let (Just rdr) = locToRdrName pos parsed
  let
#if __GLASGOW_HASKELL__ >= 800
      (Just funBind) = getHsBind pos parsed
#else
      (Just funBind) = getHsBind pos parsed
#endif
  monadifyFunBind pos nmsList funBind
  --logParsedSource "After monadification"
  addMonadToSig pos
  -- finParsed <- getRefactParsed
  --logDataWithAnns "Post monadification refactoring" finParsed
  return ()

monadifyFunBind :: SimpPos -> [GHC.Name] -> ParsedBind -> RefactGhc ()
monadifyFunBind pos nms bnd = do
  newBnd <- replaceBndRHS (modMatchRHSExpr (monadifyFunRHS nms)) bnd
  replaceFunBind pos newBnd

replaceBndRHS :: (ParsedLMatch -> RefactGhc ParsedLMatch) -> ParsedBind -> RefactGhc ParsedBind
replaceBndRHS fun fb@(GHC.FunBind {}) = SYB.everywhereM (SYB.mkM fun) fb
replaceBndRHS _ bnd = return bnd

modMatchRHSExpr :: (ParsedLExpr -> RefactGhc ParsedLExpr) -> (ParsedLMatch -> RefactGhc ParsedLMatch)
modMatchRHSExpr fun = comp
  where comp lm  =  SYB.everywhereM (SYB.mkM comp') lm
        comp' :: ParsedGRHS -> RefactGhc ParsedGRHS
#if __GLASGOW_HASKELL__ >= 806
        comp' (GHC.GRHS x lst body) = (fun body) >>= (\newBody -> return (GHC.GRHS x lst newBody))
        comp' g@(GHC.XGRHS _) = return g
#else
        comp' (GHC.GRHS lst body) = (fun body) >>= (\newBody -> return (GHC.GRHS lst newBody))
#endif

isFunCall :: [GHC.Name] -> GHC.HsExpr GhcRn -> Bool
#if __GLASGOW_HASKELL__ >= 806
isFunCall nms (GHC.HsVar _ (GHC.L _ nm2)) = elem nm2 nms
#elif __GLASGOW_HASKELL__ >= 800
isFunCall nms (GHC.HsVar (GHC.L _ nm2)) = elem nm2 nms
#else
isFunCall nms (GHC.HsVar nm2) = elem nm2 nms
#endif
#if __GLASGOW_HASKELL__ >= 806
isFunCall nms (GHC.HsPar _ e) = isFunCall nms (GHC.unLoc e)
isFunCall nms (GHC.HsApp _ lhs _) = isFunCall nms (GHC.unLoc lhs)
isFunCall nms (GHC.HsLet _ _ e) = isFunCall nms (GHC.unLoc e)
#else
isFunCall nms (GHC.HsPar e) = isFunCall nms (GHC.unLoc e)
isFunCall nms (GHC.HsApp lhs _) = isFunCall nms (GHC.unLoc lhs)
isFunCall nms (GHC.HsLet _ e) = isFunCall nms (GHC.unLoc e)
#endif
isFunCall _ _ = False

isModFunCall :: ParsedLExpr -> MonadifyState Bool
isModFunCall e = do
  renE <- lift $ lookupRenamedExpr e
  st <- get
  return $ isFunCall (funNames st) (GHC.unLoc renE)


data NameState = NS {
  num :: Int,
  name :: String
                    }

initNS :: String -> NameState
initNS nm = NS 1 nm

getNextName :: NameState -> (GHC.RdrName, NameState)
getNextName ns = (rdr, newState)
  where nameStr = (name ns) ++ (show (num ns))
        rdr = mkRdrName nameStr
        newState = ns {num = (num ns) + 1}

newName :: MonadifyState GHC.RdrName
newName = state (\s -> let n = names s
                           (rdr,ns) = getNextName n in
                         (rdr, s {names = ns}))

data MS = MS {
  names :: NameState,
  queue :: [(GHC.LPat GhcPs, ParsedLExpr)],
  funNames :: [GHC.Name]
                     }

-- showQueue :: [(GHC.LPat GhcPs, ParsedLExpr)] -> RefactGhc String
-- showQueue [] = return ""
-- showQueue ((mNm,expr):rst) = do
--   anns <- fetchAnnsFinal
--   let str = exactPrint expr anns
--   rStr <- showQueue rst
--   return $ "(" ++ (SYB.showData SYB.Parser 3 mNm) ++ ", " ++ str ++ ")\n" ++ rStr

initMS :: [GHC.Name] -> MS
initMS fns = let iNS = initNS "hare" in
  MS iNS [] fns

type MonadifyState = StateT MS RefactGhc

popQueue :: MonadifyState (GHC.LPat GhcPs, ParsedLExpr)
popQueue = state (\s -> let (e:es) = queue s
                   in (e, s {queue = es}))

--TODO the queue should eventually become [(pattern,expr)] because when
--the refactoring handles lets we'll want the argument to the lambda to
-- be the variables boind by the let

--TODO: Not sure if the queue should be a Maybe because I don't think there is a
--case where a pure function would be monadified with the >> operator
pushQueue :: (GHC.LPat GhcPs, ParsedLExpr) -> MonadifyState ()
pushQueue e = appendToQueue [e]

appendToQueue :: [(GHC.LPat GhcPs, ParsedLExpr)] -> MonadifyState ()
appendToQueue lst = state (\s -> let oldQ = queue s in
                              ((), s {queue = oldQ ++ lst}))

isQueueEmpty :: MonadifyState Bool
isQueueEmpty = get >>= (\s -> return (null (queue s)))

monadifyFunRHS :: [GHC.Name] -> ParsedLExpr -> RefactGhc ParsedLExpr
monadifyFunRHS fNames e = let initState = initMS fNames in do
  newE <- evalStateT (monadifyExpr e) initState
  return newE

-- printQueueStatus :: MonadifyState ()
-- printQueueStatus = do
--   st <- get
--   let q = queue st
--   lift $ logm ("The queue has " ++ (show $ length q) ++ " elements")
--   qStat <- lift (showQueue q)
--   lift $ logm qStat

isLet :: GHC.GenLocated l (GHC.HsExpr id) -> Bool
isLet (GHC.L _ (GHC.HsLet {})) = True
isLet _ = False

    --This function handles the top expression from the rhs of a function
monadifyExpr :: ParsedLExpr -> MonadifyState ParsedLExpr
monadifyExpr expr = do
  -- st <- get
  strippedExpr <- applyAtArgSubTrees (stripMonArgs True) expr
  isMonadCall <- isModFunCall expr
  newE <- if isMonadCall || isLet expr
    -- In this case the expression is monadic and doesn't need to be wrapped in a return
          then
            return strippedExpr
    -- Otherwise the expression needs to be wrapped with a call to return
          else
            lift $ wrapWithReturn strippedExpr
  composeWithBinds newE

lookupRenamedExpr :: ParsedLExpr -> RefactGhc (GHC.LHsExpr GhcRn)
lookupRenamedExpr parsedElem = do
  (grp,_ ,_, _) <- getRefactRenamed
  -- logData "getRenamedElem of: " parsedElem
  --logData "Renamed group: " grp
  let (strtPos,endPos) = getStartEndLoc parsedElem
      (mRenExpr :: Maybe (GHC.LHsExpr GhcRn)) = locToExp strtPos endPos (GHC.hs_valds grp)
  return $ gfromJust "lookupRenamedExpr: Renamed expression not found" mRenExpr

--Wraps the given expression with a return call
--If the expression isn't a HsPar it will be parenthisized first
wrapWithReturn :: ParsedLExpr -> RefactGhc ParsedLExpr
wrapWithReturn e@(GHC.L _ (GHC.HsPar {})) = do
#if __GLASGOW_HASKELL__ >= 806
  returnRdrL <- locate returnRdr
  addAnnVal returnRdrL
  retVar <- locate (GHC.HsVar GHC.noExt returnRdrL)
  locate (GHC.HsApp GHC.noExt retVar e)
#elif __GLASGOW_HASKELL__ >= 800
  returnRdrL <- locate returnRdr
  addAnnVal returnRdrL
  retVar <- locate (GHC.HsVar returnRdrL)
  locate (GHC.HsApp retVar e)
#else
  retVar <- locate (GHC.HsVar returnRdr)
  addAnnVal retVar
  locate (GHC.HsApp retVar e)
#endif
wrapWithReturn e@(GHC.L _ (GHC.HsVar {})) = do
#if __GLASGOW_HASKELL__ >= 806
  returnRdrL <- locate returnRdr
  addAnnVal returnRdrL
  retVar <- locate (GHC.HsVar GHC.noExt returnRdrL)
  locate (GHC.HsApp GHC.noExt retVar e)
#elif __GLASGOW_HASKELL__ >= 800
  returnRdrL <- locate returnRdr
  addAnnVal returnRdrL
  retVar <- locate (GHC.HsVar returnRdrL)
  locate (GHC.HsApp retVar e)
#else
  retVar <- locate (GHC.HsVar returnRdr)
  addAnnVal retVar
  locate (GHC.HsApp retVar e)
#endif
wrapWithReturn e = do
#if __GLASGOW_HASKELL__ >= 806
  returnRdrL <- locate returnRdr
  addAnnVal returnRdrL
  retVar <- locate (GHC.HsVar GHC.noExt returnRdrL)
#elif __GLASGOW_HASKELL__ >= 800
  returnRdrL <- locate returnRdr
  addAnnVal returnRdrL
  retVar <- locate (GHC.HsVar returnRdrL)
#else
  retVar <- locate (GHC.HsVar returnRdr)
  addAnnVal retVar
#endif
  zeroDP e
  isPared <- isWrappedInPars e
  pE <- if isPared
        then do
    setDP (DP (0,1)) e
    return e
        else wrapInPars e
#if __GLASGOW_HASKELL__ >= 806
  locate (GHC.HsApp GHC.noExt retVar pE)
#else
  locate (GHC.HsApp retVar pE)
#endif

returnRdr :: GHC.RdrName
returnRdr = mkRdrName "return"

--Takes the expressions from the queue and binds those expressions
--with a lambda expression made from the given expression and the variable from the top of the queue
composeWithBinds :: ParsedLExpr -> MonadifyState ParsedLExpr
composeWithBinds e = do
  done <- isQueueEmpty
  if done
    then return e
    else do
    (pat, lhsExpr) <- popQueue
    --Make lambda
    --lamPat <- lift $ mkVarPat var
    lamRHS <- lift $ mkGRHSs e
    lambda <- lift $ wrapInLambda pat lamRHS
    --locate + annVal bindRdr
#if __GLASGOW_HASKELL__ >= 806
    bindRdrL <- lift $ locate bindRdr
    lift $ addAnnVal bindRdrL
    lBindOp <- lift (locate (GHC.HsVar GHC.noExt bindRdrL))
#elif __GLASGOW_HASKELL__ >= 800
    bindRdrL <- lift $ locate bindRdr
    lift $ addAnnVal bindRdrL
    lBindOp <- lift (locate (GHC.HsVar bindRdrL))
#else
    lBindOp <- lift (locate (GHC.HsVar bindRdr))
    lift $ addAnnVal lBindOp
#endif
    --opApp lhsExpr locBnd lambda
#if __GLASGOW_HASKELL__ >= 806
    let opApp = (GHC.OpApp GHC.noExt lhsExpr lBindOp lambda)
#else
    let opApp = (GHC.OpApp lhsExpr lBindOp GHC.PlaceHolder lambda)
#endif
    lOpApp <- lift $ locate opApp
    composeWithBinds lOpApp

mkGRHSs :: GHC.LHsExpr GhcPs -> RefactGhc (GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs))
mkGRHSs rhsExpr = do
#if __GLASGOW_HASKELL__ >= 806
  let grhs = (GHC.GRHS GHC.noExt [] rhsExpr)
#else
  let grhs = (GHC.GRHS [] rhsExpr)
#endif
  lGrhs <- locate grhs
#if __GLASGOW_HASKELL__ >= 806
  return (GHC.GRHSs GHC.noExt [lGrhs] (GHC.noLoc (GHC.EmptyLocalBinds GHC.noExt)))
#elif __GLASGOW_HASKELL__ >= 800
  return (GHC.GRHSs [lGrhs] (GHC.noLoc GHC.EmptyLocalBinds))
#else
  return (GHC.GRHSs [lGrhs] GHC.EmptyLocalBinds)
#endif

mkVarPat :: GHC.RdrName -> RefactGhc (GHC.LPat GhcPs)
mkVarPat nm = do
#if __GLASGOW_HASKELL__ >= 806
  nmL <- locate nm
  addAnnVal nmL
  let pat = (GHC.VarPat GHC.noExt nmL) :: GHC.Pat GhcPs
#elif __GLASGOW_HASKELL__ >= 800
  nmL <- locate nm
  addAnnVal nmL
  let pat = (GHC.VarPat nmL)
#else
  let pat = (GHC.VarPat nm)
#endif
  lPat <- locate pat :: RefactGhc (GHC.LPat GhcPs)
  addAnnVal lPat
  return lPat


bindRdr :: GHC.RdrName
bindRdr = mkRdrName ">>="

--If this is called with a subtree that is a call to a monadified function
--It stores the original expression in the queue and returns a HsVar with the new name
stripMonArgs :: Bool -> ParsedLExpr -> MonadifyState ParsedLExpr
stripMonArgs wrap e = do
  st <- get
  let oldQ = queue st
  put (st {queue = []})
  stripped <- applyAtArgSubTrees (stripMonArgs wrap) e
  newQ <- get >>= (\s -> return (queue s))
  modify (\s -> s {queue = oldQ})
  isMonadCall <- isModFunCall e
  resE <- if isMonadCall && wrap
          then do
    --If this expression is a call to monad then we need to generate a new name
    --locate (HsVar newName)
    --push the new name and original expression onto the queue
    nm <- newName
#if __GLASGOW_HASKELL__ >= 806
    nmL <- lift $ locate nm
    lift $ addAnnVal nmL
    lE <- lift (locate (GHC.HsVar GHC.noExt nmL))
#elif __GLASGOW_HASKELL__ >= 800
    nmL <- lift $ locate nm
    lift $ addAnnVal nmL
    lE <- lift (locate (GHC.HsVar nmL))
#else
    lE <- lift (locWithAnnVal (GHC.HsVar nm))
#endif
    pat <- lift (mkVarPat nm)
    pushQueue (pat, e)
    return lE
          else
    --otherwise just return the stripped expression
            return stripped
  --If the whole expression is monadic then it needs to be added to the queue
  --before the monadic calls in this expression's arguments
  appendToQueue newQ
  return resE


--Takes in an expression that should represent either a function application or an operator applicator
--The function will be applied to each argument subtree
applyAtArgSubTrees :: (ParsedLExpr -> MonadifyState ParsedLExpr) -> ParsedLExpr -> MonadifyState ParsedLExpr
#if __GLASGOW_HASKELL__ >= 806
applyAtArgSubTrees f (GHC.L l (GHC.HsApp x lhs@(GHC.L _ (GHC.HsVar {})) rhs))
  = f rhs >>= (\newRhs -> return (GHC.L l (GHC.HsApp x lhs newRhs)))
applyAtArgSubTrees f (GHC.L l (GHC.HsPar x e)) = do
  newE <- applyAtArgSubTrees f e
  return (GHC.L l (GHC.HsPar x newE))
applyAtArgSubTrees f (GHC.L l (GHC.HsApp x lhs rhs)) = do
  newLhs <- applyAtArgSubTrees f lhs
  newRhs <- f rhs
  return (GHC.L l (GHC.HsApp x newLhs newRhs))
#else
applyAtArgSubTrees f (GHC.L l (GHC.HsApp lhs@(GHC.L _ (GHC.HsVar _)) rhs))
  = f rhs >>= (\newRhs -> return (GHC.L l (GHC.HsApp lhs newRhs)))
applyAtArgSubTrees f (GHC.L l (GHC.HsPar e)) = do
  newE <- applyAtArgSubTrees f e
  return (GHC.L l (GHC.HsPar newE))
applyAtArgSubTrees f (GHC.L l (GHC.HsApp lhs rhs)) = do
  newLhs <- applyAtArgSubTrees f lhs
  newRhs <- f rhs
  return (GHC.L l (GHC.HsApp newLhs newRhs))
#endif
#if __GLASGOW_HASKELL__ >= 806
applyAtArgSubTrees f (GHC.L l (GHC.HsLet x _locBnds expr)) = do
#else
applyAtArgSubTrees f (GHC.L l (GHC.HsLet _locBnds expr)) = do
#endif
  renamed <- lift getRefactRenamed
  let renExpr = getNamedExprByPos l renamed
  mBnds <- filterBinds renExpr
  newExpr <- f expr
  isMonExpr <- isModFunCall newExpr
  rE <- if isMonExpr
        then return newExpr
        else lift $ wrapWithReturn newExpr
  case mBnds of
#if __GLASGOW_HASKELL__ >= 806
    Just bnds -> do
      bndsL <- lift $ locate bnds
      return (GHC.L l (GHC.HsLet x bndsL rE))
#elif __GLASGOW_HASKELL__ >= 800
    Just bnds -> do
      bndsL <- lift $ locate bnds
      return (GHC.L l (GHC.HsLet bndsL rE))
#else
    Just bnds -> return (GHC.L l (GHC.HsLet bnds rE))
#endif
    Nothing -> return rE
applyAtArgSubTrees _ ast = return ast

getNamedExprByPos :: (Data a) => GHC.SrcSpan -> a -> GHC.LHsExpr GhcRn
getNamedExprByPos pos a = let res = SYB.something (Nothing `SYB.mkQ` comp) a in
  gfromJust "getNamedExprByPos: No expression found." res
  where comp ::  GHC.LHsExpr GhcRn -> Maybe (GHC.LHsExpr GhcRn)
        comp e@(GHC.L l _) = if l == pos
                             then (Just e)
                             else Nothing

-- getParsedExprByPos :: (Data a) => GHC.SrcSpan -> a -> ParsedLExpr
-- getParsedExprByPos = getParsedByPos

getParsedBindByPos :: (Data a) => GHC.SrcSpan -> a -> GHC.LHsBind GhcPs
getParsedBindByPos = getParsedByPos

getParsedByPos :: (Data t) => GHC.SrcSpan -> t -> GHC.LHsBind GhcPs
getParsedByPos pos a = let res = SYB.something (Nothing `SYB.mkQ` comp) a in
  gfromJust "getNamedExprByPos: No expression found." res
  where comp e@(GHC.L l _) = if l == pos
                             then (Just e)
                             else Nothing

-- TODO:AZ: why is this using Renamed Source?
-- TODO:AZ: why is this discarding signatures?
filterBinds :: GHC.LHsExpr GhcRn -> MonadifyState (Maybe (GHC.HsLocalBinds GhcPs))
#if __GLASGOW_HASKELL__ >= 806
filterBinds (GHC.L _ (GHC.HsLet _ (GHC.L _ locBnds) _)) = case locBnds of
  (GHC.HsValBinds _ (GHC.XValBindsLR (GHC.NValBinds lst _))) -> do
    newLst <- mMapMaybe handleBind (reverse lst)
    case newLst of
      [] -> return Nothing
      lst' -> return (Just (GHC.HsValBinds GHC.noExt (GHC.ValBinds GHC.noExt (GHC.listToBag (reverse lst')) [])))
  (GHC.HsValBinds _ (GHC.ValBinds {})) -> error "filterBinds:invalid ValBinds"
  (GHC.EmptyLocalBinds _) -> return Nothing
  (GHC.HsIPBinds _ _)     -> return Nothing
  (GHC.XHsLocalBindsLR _) -> return Nothing
#elif __GLASGOW_HASKELL__ >= 800
filterBinds (GHC.L _ (GHC.HsLet (GHC.L _ locBnds) _)) = case locBnds of
  (GHC.HsValBinds (GHC.ValBindsOut lst _)) -> do
    newLst <- mMapMaybe handleBind (reverse lst)
    case newLst of
      [] -> return Nothing
      lst -> return (Just (GHC.HsValBinds (GHC.ValBindsIn (GHC.listToBag (reverse lst)) [])))
#else
filterBinds (GHC.L _ (GHC.HsLet locBnds _)) = case locBnds of
  (GHC.HsValBinds (GHC.ValBindsOut lst _)) -> do
    newLst <- mMapMaybe handleBind (reverse lst)
    case newLst of
      [] -> return Nothing
      lst -> return (Just (GHC.HsValBinds (GHC.ValBindsIn (GHC.listToBag (reverse lst)) [])))
#endif

filterBinds _ = error "filterBinds: Called with a non-let expression."

{- This is what handles an expression on the rhs of a let
   any monadic calls inside of the expression are stripped and added to the queue before
   the top level let expression

Assuming mon and mon2 are monadic
let f = mon (mon2 4) 5 in
expr ===>
(mon2 4) >>= (\hare1 -> mon hare1 5 >>= (f -> expr))
-}

handleBind :: (GHC.RecFlag, GHC.LHsBinds GhcRn) -> MonadifyState (Maybe (GHC.LHsBindLR GhcPs GhcPs))
handleBind (_, bg) = do
  let [(GHC.L ll _bnd)] = GHC.bagToList bg
  parsed <- lift getRefactParsed
  let pBnd = getParsedBindByPos ll parsed
  case pBnd of
    (LL l fb@(GHC.FunBind { GHC.fun_id = (GHC.L _ n), GHC.fun_matches = mg })) -> do
      let expr = getExprFromMG mg
      isModCall <- isModFunCall expr
      newExpr <- stripMonArgs False expr
      if isModCall
        then do
        pat <- lift (mkVarPat n)
        pushQueue (pat,newExpr)
        return Nothing
        else return (Just (LL l (fb {GHC.fun_matches = replaceMGExpr newExpr mg})))
    (LL l pb@(GHC.PatBind { GHC.pat_lhs = pat, GHC.pat_rhs = grhss })) -> do
      let expr = getExprFromGRHSs grhss
      newExpr <- stripMonArgs False expr
      isModCall <- isModFunCall expr
      if isModCall
        then do
        pushQueue (pat, newExpr)
        return Nothing
        else return (Just (LL l (pb {GHC.pat_rhs = replaceGRHSsExpr newExpr grhss})))


replaceMGExpr :: ParsedLExpr -> ParsedMatchGroup -> ParsedMatchGroup
replaceMGExpr e = SYB.everywhere (SYB.mkT (replaceGRHSExpr e))

replaceGRHSsExpr :: ParsedLExpr -> ParsedGRHSs -> ParsedGRHSs
replaceGRHSsExpr e = SYB.everywhere (SYB.mkT (replaceGRHSExpr e))

replaceGRHSExpr :: ParsedLExpr -> ParsedGRHS -> ParsedGRHS
#if __GLASGOW_HASKELL__ >= 806
replaceGRHSExpr e (GHC.GRHS x stmts _) = (GHC.GRHS x stmts e)
#else
replaceGRHSExpr e (GHC.GRHS stmts _) = (GHC.GRHS stmts e)
#endif
replaceGRHSExpr _ grhs = grhs

getExprFromGRHSs :: ParsedGRHSs -> ParsedLExpr
getExprFromGRHSs grhs = let res = SYB.something (Nothing `SYB.mkQ` comp) grhs in
  gfromJust "getExprFromMG" res
  where comp :: ParsedLExpr -> Maybe ParsedLExpr
        comp e = Just e

getExprFromMG :: ParsedMatchGroup -> ParsedLExpr
getExprFromMG mg = let res = SYB.something (Nothing `SYB.mkQ` comp) mg in
  gfromJust "getExprFromMG" res
  where comp :: ParsedLExpr -> Maybe ParsedLExpr
        comp e = Just e


mMapMaybe :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mMapMaybe _ [] = return []
mMapMaybe f (x:xs) = do
  mElem <- f x
  rst <- mMapMaybe f xs
  case mElem of
    Nothing -> return rst
    (Just el) -> return (el:rst)


getTypeSigByName :: (Data t) => GHC.RdrName -> t -> Maybe (GHC.Sig GhcPs)
getTypeSigByName nm t = SYB.something (Nothing `SYB.mkQ` comp) t
  where comp :: GHC.Sig GhcPs -> Maybe (GHC.Sig GhcPs)
#if __GLASGOW_HASKELL__ >= 806
        comp s@(GHC.TypeSig _ [(GHC.L _ x)] _) = if nm == x
#elif __GLASGOW_HASKELL__ >= 800
        comp s@(GHC.TypeSig [(GHC.L _ x)] _) = if nm == x
#else
        comp s@(GHC.TypeSig [(GHC.L _ x)] _ _) = if nm == x
#endif
                                       then Just s
                                       else Nothing
        comp _ = Nothing


--This adds the monad class constraint to the front of a
--binds type signature and wraps the result type with the monad type variable
--It may also not affect anything if there is no signature found
addMonadToSig :: SimpPos -> RefactGhc ()
addMonadToSig pos = do
  parsed <- getRefactParsed
  let (Just (GHC.L _ rdrNm)) = locToRdrName pos parsed
      tySig = getTypeSigByName rdrNm parsed
  logDataWithAnns "addMonadToSig:tySig" tySig
  case tySig of
    (Just sig) -> do
      newSig <- modTySig sig
      replaceTypeSig pos newSig
    Nothing -> do
      logm $ "addMonadToSig: sig not found"
      return ()
    where modTySig :: GHC.Sig GhcPs -> RefactGhc (GHC.Sig GhcPs)
#if __GLASGOW_HASKELL__ >= 806
          modTySig (GHC.TypeSig x nms (GHC.HsWC wcs (GHC.HsIB a ty)))
             = (modType ty) >>= (\nTy -> return (GHC.TypeSig x nms (GHC.HsWC wcs (GHC.HsIB a nTy))))
#elif __GLASGOW_HASKELL__ >= 802
          modTySig (GHC.TypeSig nms (GHC.HsWC wcs (GHC.HsIB a ty b)))
             = (modType ty) >>= (\nTy -> return (GHC.TypeSig nms (GHC.HsWC wcs (GHC.HsIB a nTy b))))
#elif __GLASGOW_HASKELL__ >= 800
          modTySig (GHC.TypeSig names (GHC.HsIB pvs (GHC.HsWC pns wcs ty)))
             = (modType ty) >>= (\nTy -> return (GHC.TypeSig names (GHC.HsIB pvs (GHC.HsWC pns wcs nTy))))
#else
          modTySig (GHC.TypeSig nms ty pstRn) = (modType ty)  >>= (\nTy -> return (GHC.TypeSig nms nTy pstRn))
#endif
          modTySig _ = error "addMonadToSig: modTySig called with an unknown constructor."

          modType :: GHC.LHsType GhcPs -> RefactGhc (GHC.LHsType GhcPs)
#if __GLASGOW_HASKELL__ >= 806
          modType (GHC.L l (GHC.HsQualTy x (GHC.L l2 cntxt) ty)) = do
            lMonad <- locate (mkRdrName "Monad")
            lMonTy <- locWithAnnVal (GHC.HsTyVar GHC.noExt GHC.NotPromoted lMonad)
            zeroDP lMonTy
            lm <- locate (mkRdrName "m")
            lVarTy <- locWithAnnVal (GHC.HsTyVar GHC.noExt GHC.NotPromoted lm)
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy GHC.noExt lMonTy lVarTy)
            lAppTy <- locWithAnnVal appTy
            zeroDP lAppTy
            newCntxt <- if (null cntxt)
                        then do
              lCntxt <- locate [lAppTy]
              (addNewKeyword (G GHC.AnnDarrow, DP (0,1)) lCntxt)
              zeroDP lCntxt
              return lCntxt
                        else return (GHC.L l2 (lAppTy:cntxt))
            setDP  (DP (0,1)) newTy
            return (GHC.L l (GHC.HsQualTy x newCntxt newTy))
          modType (GHC.L l (GHC.HsForAllTy x bndrs ty)) = do
            lMonad <- locate (mkRdrName "Monad")
            lMonTy <- locWithAnnVal (GHC.HsTyVar GHC.noExt GHC.NotPromoted lMonad)
            zeroDP lMonTy
            lm <- locate (mkRdrName "m")
            lVarTy <- locWithAnnVal (GHC.HsTyVar GHC.noExt GHC.NotPromoted lm) :: RefactGhc (GHC.LHsType GhcPs)
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy GHC.noExt lMonTy lVarTy)
            lAppTy <- locWithAnnVal appTy :: RefactGhc (GHC.LHsType GhcPs)
            zeroDP lAppTy
            setDP  (DP (0,1)) newTy
            return (GHC.L l (GHC.HsForAllTy x bndrs newTy))
          modType ty = do
            lMonad <- locate (mkRdrName "Monad")
            addAnnVal lMonad
            lMonTy <- locate (GHC.HsTyVar GHC.noExt GHC.NotPromoted lMonad)
            -- zeroDP lMonTy
            lm <- locate (mkRdrName "m")
            addAnnVal lm
            lVarTy <- locate (GHC.HsTyVar GHC.noExt GHC.NotPromoted lm)
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy GHC.noExt lMonTy lVarTy) :: GHC.HsType GhcPs
            lAppTy <- locWithAnnVal appTy
            zeroDP lAppTy
            newCntxt <- do
              lCntxt <- locate [lAppTy]
              (addNewKeyword (G GHC.AnnDarrow, DP (0,1)) lCntxt)
              zeroDP lCntxt
              return lCntxt
            setDP  (DP (0,1)) newTy
            retTy <- locate (GHC.HsQualTy GHC.noExt newCntxt newTy)
            return retTy
#elif __GLASGOW_HASKELL__ >= 802
          modType (GHC.L l (GHC.HsQualTy (GHC.L l2 cntxt) ty)) = do
            lMonad <- locate (mkRdrName "Monad")
            lMonTy <- locWithAnnVal (GHC.HsTyVar GHC.NotPromoted lMonad)
            zeroDP lMonTy
            lm <- locate (mkRdrName "m")
            lVarTy <- locWithAnnVal (GHC.HsTyVar GHC.NotPromoted lm)
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy lMonTy lVarTy)
            lAppTy <- locWithAnnVal appTy
            zeroDP lAppTy
            newCntxt <- if (null cntxt)
                        then do
              lCntxt <- locate [lAppTy]
              (addNewKeyword (G GHC.AnnDarrow, DP (0,1)) lCntxt)
              zeroDP lCntxt
              return lCntxt
                        else return (GHC.L l2 (lAppTy:cntxt))
            setDP  (DP (0,1)) newTy
            return (GHC.L l (GHC.HsQualTy newCntxt newTy))
          modType (GHC.L l (GHC.HsForAllTy bndrs ty)) = do
            lMonad <- locate (mkRdrName "Monad")
            lMonTy <- locWithAnnVal (GHC.HsTyVar GHC.NotPromoted lMonad)
            zeroDP lMonTy
            lm <- locate (mkRdrName "m")
            lVarTy <- locWithAnnVal (GHC.HsTyVar GHC.NotPromoted lm) :: RefactGhc (GHC.LHsType GhcPs)
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy lMonTy lVarTy)
            lAppTy <- locWithAnnVal appTy
            zeroDP lAppTy
            setDP  (DP (0,1)) newTy
            return (GHC.L l (GHC.HsForAllTy bndrs newTy))
          modType ty = do
            lMonad <- locate (mkRdrName "Monad")
            addAnnVal lMonad
            lMonTy <- locate (GHC.HsTyVar GHC.NotPromoted lMonad)
            -- zeroDP lMonTy
            lm <- locate (mkRdrName "m")
            addAnnVal lm
            lVarTy <- locate (GHC.HsTyVar GHC.NotPromoted lm)
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy lMonTy lVarTy)
            lAppTy <- locWithAnnVal appTy
            zeroDP lAppTy
            newCntxt <- do
              lCntxt <- locate [lAppTy]
              (addNewKeyword (G GHC.AnnDarrow, DP (0,1)) lCntxt)
              zeroDP lCntxt
              return lCntxt
            setDP  (DP (0,1)) newTy
            retTy <- locate (GHC.HsQualTy newCntxt newTy)
            return retTy
#elif __GLASGOW_HASKELL__ >= 800
          modType (GHC.L l (GHC.HsQualTy (GHC.L l2 cntxt) ty)) = do
            lMonad <- locate (mkRdrName "Monad")
            lMonTy <- locWithAnnVal (GHC.HsTyVar lMonad)
            zeroDP lMonTy
            lm <- locate (mkRdrName "m")
            lVarTy <- locWithAnnVal (GHC.HsTyVar lm)
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy lMonTy lVarTy)
            lAppTy <- locWithAnnVal appTy
            zeroDP lAppTy
            newCntxt <- if (null cntxt)
                        then do
              lCntxt <- locate [lAppTy]
              (addNewKeyword (G GHC.AnnDarrow, DP (0,1)) lCntxt)
              zeroDP lCntxt
              return lCntxt
                        else return (GHC.L l2 (lAppTy:cntxt))
            setDP  (DP (0,1)) newTy
            return (GHC.L l (GHC.HsQualTy newCntxt newTy))
          modType (GHC.L l (GHC.HsForAllTy bndrs ty)) = do
            lMonad <- locate (mkRdrName "Monad")
            lMonTy <- locWithAnnVal (GHC.HsTyVar lMonad)
            zeroDP lMonTy
            lm <- locate (mkRdrName "m")
            lVarTy <- locWithAnnVal (GHC.HsTyVar lm)
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy lMonTy lVarTy)
            lAppTy <- locWithAnnVal appTy
            zeroDP lAppTy
            setDP  (DP (0,1)) newTy
            return (GHC.L l (GHC.HsForAllTy bndrs newTy))
          modType ty = do
            lMonad <- locate (mkRdrName "Monad")
            addAnnVal lMonad
            lMonTy <- locate (GHC.HsTyVar lMonad)
            -- zeroDP lMonTy
            lm <- locate (mkRdrName "m")
            addAnnVal lm
            lVarTy <- locate (GHC.HsTyVar lm)
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy lMonTy lVarTy)
            lAppTy <- locWithAnnVal appTy
            zeroDP lAppTy
            newCntxt <- do
              lCntxt <- locate [lAppTy]
              (addNewKeyword (G GHC.AnnDarrow, DP (0,1)) lCntxt)
              zeroDP lCntxt
              return lCntxt
            setDP  (DP (0,1)) newTy
            retTy <- locate (GHC.HsQualTy newCntxt newTy)
            return retTy
#else
          modType (GHC.L l (GHC.HsForAllTy flg mSpn bndrs (GHC.L l2 cntxt) ty)) = do
            lMonTy <- locWithAnnVal (GHC.HsTyVar (mkRdrName "Monad"))
            zeroDP lMonTy
            lVarTy <- locWithAnnVal (GHC.HsTyVar (mkRdrName "m"))
            newTy <- wrapResTy (mkRdrName "m") ty
            let appTy = (GHC.HsAppTy lMonTy lVarTy)
            lAppTy <- locWithAnnVal appTy
            zeroDP lAppTy
            newCntxt <- if (null cntxt)
                        then do
              lCntxt <- locate [lAppTy]
              (addNewKeyword (G GHC.AnnDarrow, DP (0,1)) lCntxt)
              zeroDP lCntxt
              return lCntxt
                        else return (GHC.L l2 (lAppTy:cntxt))
            setDP  (DP (0,1)) newTy
            return (GHC.L l (GHC.HsForAllTy flg mSpn bndrs newCntxt newTy))
#endif
          wrapResTy :: GHC.RdrName -> GHC.LHsType GhcPs -> RefactGhc (GHC.LHsType GhcPs)
#if __GLASGOW_HASKELL__ >= 806
          wrapResTy rdr (GHC.L l (GHC.HsFunTy x lTy rTy)) = do
            nRTy <- wrapResTy rdr rTy
            return (GHC.L l (GHC.HsFunTy x lTy nRTy))
#else
          wrapResTy rdr (GHC.L l (GHC.HsFunTy lTy rTy)) = do
            nRTy <- wrapResTy rdr rTy
            return (GHC.L l (GHC.HsFunTy lTy nRTy))
#endif
#if __GLASGOW_HASKELL__ >= 806
          wrapResTy rdr locTy = do
            rdrL <- locate rdr
            addAnnVal rdrL
            lTyVar <- locate (GHC.HsTyVar GHC.noExt GHC.NotPromoted rdrL)
            locate (GHC.HsAppTy GHC.noExt lTyVar locTy)
#elif __GLASGOW_HASKELL__ >= 802
          wrapResTy rdr locTy = do
            rdrL <- locate rdr
            addAnnVal rdrL
            lTyVar <- locate (GHC.HsTyVar GHC.NotPromoted rdrL)
            locate (GHC.HsAppTy lTyVar locTy)
#elif __GLASGOW_HASKELL__ >= 800
          wrapResTy rdr locTy = do
            rdrL <- locate rdr
            addAnnVal rdrL
            lTyVar <- locate (GHC.HsTyVar rdrL)
            locate (GHC.HsAppTy lTyVar locTy)
#else
          wrapResTy rdr locTy = locWithAnnVal (GHC.HsTyVar rdr) >>= (\ lTyVar -> locate (GHC.HsAppTy lTyVar locTy))
#endif
