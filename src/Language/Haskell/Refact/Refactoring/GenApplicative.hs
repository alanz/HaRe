{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Refact.Refactoring.GenApplicative
  (genApplicative, compGenApplicative) where

import Language.Haskell.Refact.API
import qualified GhcModCore   as GM (Options(..))
import qualified GHC as GHC
import qualified RdrName as GHC
import System.Directory
import FastString
import Data.Generics as SYB
import Data.List
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Parsers

genApplicative :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]
genApplicative settings cradle fileName pos = do
  absFileName <- normaliseFilePath fileName
  runRefacSession settings cradle (compGenApplicative absFileName pos)

compGenApplicative :: FilePath -> SimpPos -> RefactGhc [ApplyRefacResult]
compGenApplicative fileName pos = do
  (refRes@((_fp,ismod),_), ()) <- applyRefac (doGenApplicative fileName pos) (RSFile fileName)
  case ismod of
    RefacUnmodifed -> error "Generalise to Applicative failed"
    RefacModified -> return ()
  return [refRes]

--The general operation of this refactoring involves transposing the LHsExpr's from within the list of ExprLStmt's
-- to an (OpApp :: HsExpr)
-- The function begins by constructing the beginning of the applicative chain by looking at the construction of the return statement

doGenApplicative :: FilePath -> SimpPos -> RefactGhc ()
doGenApplicative fileName pos = do
  parsed <- getRefactParsed
  let (Just lrdr) = locToRdrName pos parsed
#if __GLASGOW_HASKELL__ >= 800
      funBind = case getHsBind pos parsed of
#else
      funBind = case getHsBind pos parsed of
#endif
                  (Just fb) -> fb
                  Nothing -> error "That location is not the start of a function"
      (retRhs, doStmts) = case (getReturnRhs funBind, getDoStmts funBind) of
                            ((Just rets), (Just stmt)) -> (rets, stmt)
                            _ -> error "The function needs to consist of set of do statements with a return value."
      boundVars = findBoundVars doStmts
  checkPreconditions retRhs doStmts boundVars
  appChain <- constructAppChain retRhs doStmts
  replaceFunRhs pos appChain


checkPreconditions :: ParsedExpr -> [GHC.ExprLStmt GhcPs] -> [GHC.RdrName] -> RefactGhc ()
checkPreconditions retRhs doStmts boundVars = do
  let boundVarsPrecon = checkBVars doStmts boundVars
      retVarsOrder = varOrdering boundVars retRhs
      orderingPrecon = checkOrdering retVarsOrder doStmts
  if (not boundVarsPrecon)
    then error "GenApplicative Precondition: The function given uses a bound variable in a RHS expression."
    else if (not orderingPrecon)
         then error "GenApplicative Precondition: Variables are not bound in the order that they appear in the return statement."
         else return ()
  where checkBVars [] _ = True
        checkBVars (stmt:stmts) vars = case stmt of
#if __GLASGOW_HASKELL__ >= 806
          (GHC.L _ (GHC.BodyStmt _ body _ _)) -> (not (lexprContainsVars vars body)) && (checkBVars stmts vars)
#else
          (GHC.L _ (GHC.BodyStmt body _ _ _)) -> (not (lexprContainsVars vars body)) && (checkBVars stmts vars)
#endif
#if __GLASGOW_HASKELL__ >= 806
          (GHC.L _ (GHC.BindStmt _ _ body _ _)) -> (not (lexprContainsVars vars body)) && (checkBVars stmts vars)
#elif __GLASGOW_HASKELL__ > 710
          (GHC.L _ (GHC.BindStmt _ body _ _ _)) -> (not (lexprContainsVars vars body)) && (checkBVars stmts vars)
#else
          (GHC.L _ (GHC.BindStmt _ body _ _)) -> (not (lexprContainsVars vars body)) && (checkBVars stmts vars)
#endif
        lexprContainsVars :: [GHC.RdrName] -> ParsedLExpr -> Bool
        lexprContainsVars vars = SYB.everything (||) (False `SYB.mkQ` (\nm -> elem nm vars))
        varOrdering :: [GHC.RdrName] -> ParsedExpr -> [GHC.RdrName]
        varOrdering boundVars = SYB.everything (++) ([] `SYB.mkQ` (\nm -> if (elem nm boundVars) then [nm] else []))
        checkOrdering :: [GHC.RdrName] -> [GHC.ExprLStmt GhcPs] -> Bool
        checkOrdering [] [] = True
        checkOrdering [] ((GHC.L _ (GHC.BodyStmt _ _ _ _)):stmts) = checkOrdering [] stmts
        checkOrdering vars ((GHC.L _ (GHC.BodyStmt _ _ _ _)):stmts) = checkOrdering vars stmts
#if __GLASGOW_HASKELL__ >= 806
        checkOrdering (var:vars) ((GHC.L _ (GHC.BindStmt _ pat _ _ _)):stmts) =
#elif __GLASGOW_HASKELL__ > 710
        checkOrdering (var:vars) ((GHC.L _ (GHC.BindStmt pat _ _ _ _)):stmts) =
#else
        checkOrdering (var:vars) ((GHC.L _ (GHC.BindStmt pat _ _ _)):stmts) =
#endif
          if (checkPat var pat)
          then (checkOrdering vars stmts)
          else False
        checkPat var pat = gContains var pat

gContains :: (Data t, Eq a, Data a) => a -> t -> Bool
gContains item t = SYB.everything (||) (False `SYB.mkQ` (\b -> item == b)) t

processReturnStatement :: ParsedExpr -> [GHC.RdrName] -> RefactGhc (Maybe ParsedLExpr)
processReturnStatement retExpr boundVars
  | isJustBoundVar retExpr boundVars = return Nothing
  | otherwise =
      case retExpr of
#if __GLASGOW_HASKELL__ >= 806
        (GHC.ExplicitTuple _ lst _) -> do
#else
        (GHC.ExplicitTuple lst _) -> do
#endif
          dFlags <- GHC.getSessionDynFlags
          let commas = repeat ','
              constr = "(" ++ (take ((length lst)-1) commas) ++ ")"
              parseRes = parseExpr dFlags "hare" constr
          case parseRes of
            (Left (_, errMsg)) -> do
              logm "processReturnStatement: error parsing tuple constructor"
              return Nothing
            (Right (anns, expr)) -> do
              mergeRefactAnns anns
              return (Just expr)
        _ -> do
          lRet <- locate retExpr
          stripBoundVars lRet boundVars
      where stripBoundVars :: ParsedLExpr -> [GHC.RdrName] -> RefactGhc (Maybe ParsedLExpr)
#if __GLASGOW_HASKELL__ >= 806
            stripBoundVars le@(GHC.L l (GHC.HsVar _ (GHC.L _ nm))) names =
#elif __GLASGOW_HASKELL__ > 710
            stripBoundVars le@(GHC.L l (GHC.HsVar (GHC.L _ nm))) names =
#else
            stripBoundVars le@(GHC.L l (GHC.HsVar nm)) names =
#endif
              if (elem nm names)
              then return Nothing
              else return (Just le)
#if __GLASGOW_HASKELL__ >= 806
            stripBoundVars (GHC.L l (GHC.HsApp x expr1 expr2)) names = do
#else
            stripBoundVars (GHC.L l (GHC.HsApp expr1 expr2)) names = do
#endif
              ne1 <- stripBoundVars expr1 names
              ne2 <- stripBoundVars expr2 names
              case ne2 of
                Nothing -> return ne1
#if __GLASGOW_HASKELL__ >= 806
                (Just e2) -> return (ne1 >>= (\e1 -> Just (GHC.L l (GHC.HsApp x e1 e2))))
#else
                (Just e2) -> return (ne1 >>= (\e1 -> Just (GHC.L l (GHC.HsApp e1 e2))))
#endif

isJustBoundVar :: ParsedExpr -> [GHC.RdrName] -> Bool
#if __GLASGOW_HASKELL__ >= 806
isJustBoundVar (GHC.HsVar _ (GHC.L _ nm)) names = elem nm names
#elif __GLASGOW_HASKELL__ > 710
isJustBoundVar (GHC.HsVar (GHC.L _ nm)) names = elem nm names
#else
isJustBoundVar (GHC.HsVar nm) names = elem nm names
#endif
isJustBoundVar _ _ = False

getDoStmts :: GHC.HsBind GhcPs -> Maybe [GHC.ExprLStmt GhcPs]
getDoStmts funBind = SYB.something (Nothing `SYB.mkQ` stmtLst) funBind
  where stmtLst :: GHC.HsExpr GhcPs -> Maybe [GHC.ExprLStmt GhcPs]
#if __GLASGOW_HASKELL__ >= 806
        stmtLst (GHC.HsDo _ _ (GHC.L _ stmtLst) ) = Just (init stmtLst)
#elif __GLASGOW_HASKELL__ > 710
        stmtLst (GHC.HsDo _ (GHC.L _ stmtLst) _) = Just (init stmtLst)
#else
        stmtLst (GHC.HsDo _ stmtLst _) = Just (init stmtLst)
#endif
        stmtLst _ = Nothing

findBoundVars :: [GHC.ExprLStmt GhcPs] -> [GHC.RdrName]
findBoundVars = SYB.everything (++) ([] `SYB.mkQ` findVarPats)
  where findVarPats :: GHC.Pat GhcPs -> [GHC.RdrName]
#if __GLASGOW_HASKELL__ >= 806
        findVarPats (GHC.VarPat _ (GHC.L _ rdr)) = [rdr]
#elif __GLASGOW_HASKELL__ > 710
        findVarPats (GHC.VarPat (GHC.L _ rdr)) = [rdr]
#else
        findVarPats (GHC.VarPat rdr) = [rdr]
#endif
        findVarPats _ = []

getReturnRhs :: UnlocParsedHsBind -> Maybe ParsedExpr
getReturnRhs funBind = SYB.something (Nothing `SYB.mkQ` retStmt `SYB.extQ` dollarRet) funBind
  where retStmt :: GHC.ExprLStmt GhcPs -> Maybe ParsedExpr
#if __GLASGOW_HASKELL__ >= 806
        retStmt (GHC.L _ (GHC.BodyStmt _ (GHC.L _ body)  _ _)) = if isRet body
#else
        retStmt (GHC.L _ (GHC.BodyStmt (GHC.L _ body)  _ _ _)) = if isRet body
#endif
          then Just (retRHS body)
          else Nothing
        retStmt _ = Nothing
        dollarRet :: ParsedExpr -> Maybe ParsedExpr
#if __GLASGOW_HASKELL__ >= 806
        dollarRet (GHC.OpApp _ ret dollar expr)
#else
        dollarRet (GHC.OpApp ret dollar _ expr)
#endif
          = if (isHsVar "return" $ GHC.unLoc ret) && (isHsVar "$" $ GHC.unLoc dollar)
              then Just (GHC.unLoc expr)
              else Nothing
        dollarRet _ = Nothing

#if __GLASGOW_HASKELL__ >= 806
        isRet :: ParsedExpr -> Bool
        isRet (GHC.HsApp _ (GHC.L _ mRet) _) = isHsVar "return" mRet
        isRet _ = False
        retRHS :: ParsedExpr -> ParsedExpr
        retRHS (GHC.HsApp _ _  (GHC.L _ rhs)) = rhs
#else
        isRet :: ParsedExpr -> Bool
        isRet (GHC.HsApp (GHC.L _ mRet) _) = isHsVar "return" mRet
        isRet _ = False
        retRHS :: ParsedExpr -> ParsedExpr
        retRHS (GHC.HsApp _  (GHC.L _ rhs)) = rhs
#endif

constructAppChain :: ParsedExpr -> [GHC.ExprLStmt GhcPs] -> RefactGhc ParsedLExpr
constructAppChain retRhs lst = do
  let clusters = clusterStmts lst
      boundVars = findBoundVars lst
  pars <- mapM buildSingleExpr clusters
  pars2 <- if length pars == 1
              then do newP <- (removePars (head pars))
                      return [newP]
              else return pars
  effects <- buildChain pars2
  mPure <- processReturnStatement retRhs boundVars
  case mPure of
    Nothing -> do
      return effects
    (Just pure) -> do
      setDP (DP (0,1)) pure
      lOp <- lInfixFmap
      addAnnVal lOp
#if __GLASGOW_HASKELL__ >= 806
      locate (GHC.OpApp GHC.noExt pure lOp effects)
#else
      locate (GHC.OpApp pure lOp GHC.PlaceHolder effects)
#endif
  where
    buildChain :: [ParsedLExpr] -> RefactGhc ParsedLExpr
    buildChain [e] = return e
    buildChain (e:es) = do
      rhs <- buildChain es
      lOp <- lFApp
      addAnnVal lOp
#if __GLASGOW_HASKELL__ >= 806
      let opApp = (GHC.OpApp GHC.noExt e lOp rhs)
#else
      let opApp = (GHC.OpApp e lOp GHC.PlaceHolder rhs)
#endif
      locate opApp
    getStmtExpr :: GHC.ExprLStmt GhcPs -> ParsedLExpr
#if __GLASGOW_HASKELL__ >= 806
    getStmtExpr (GHC.L _ (GHC.BodyStmt _ body _ _)) = body
#else
    getStmtExpr (GHC.L _ (GHC.BodyStmt body _ _ _)) = body
#endif
#if __GLASGOW_HASKELL__ >= 806
    getStmtExpr (GHC.L _ (GHC.BindStmt _ _ body _ _)) = body
#elif __GLASGOW_HASKELL__ > 710
    getStmtExpr (GHC.L _ (GHC.BindStmt _ body _ _ _)) = body
#else
    getStmtExpr (GHC.L _ (GHC.BindStmt _ body _ _)) = body
#endif
    buildSingleExpr :: [GHC.ExprLStmt GhcPs] -> RefactGhc ParsedLExpr
    buildSingleExpr [st] = return $ getStmtExpr st
    buildSingleExpr lst@(st:stmts) = do
      let (before,(bindSt:after)) = break isBindStmt lst
      rOp <- rApp
      lOp <- lApp
      mLeftOfBnds <- buildApps rOp (map getStmtExpr before)
      mRightOfBnds <- buildApps lOp (map getStmtExpr after)
      mapM_ (\ex -> (setDP (DP (0,1))) (getStmtExpr ex)) (tail lst)
      lROp <- lRApp
      addAnnVal lROp
      lLOp <- lLApp
      addAnnVal lLOp
      newBndStmt <- mkBind (getStmtExpr bindSt)
#if __GLASGOW_HASKELL__ >= 806
      case (mLeftOfBnds,mRightOfBnds) of
        (Nothing,Nothing) -> error "buildSingleExpr was passed an empty list."
        ((Just leftOfBnds),Nothing) -> do
          app <- locate (GHC.OpApp GHC.noExt leftOfBnds lROp newBndStmt)
          wrapInPars app
        (Nothing, (Just rightOfBnds)) -> do
          app <- locate (GHC.OpApp GHC.noExt newBndStmt lLOp rightOfBnds)
          wrapInPars app
        ((Just leftOfBnds),(Just rightOfBnds)) -> do
          setDP (DP (0,1)) newBndStmt
          lOpApp <- locate (GHC.OpApp GHC.noExt leftOfBnds lROp newBndStmt)
          fullApp <- locate (GHC.OpApp GHC.noExt lOpApp lLOp rightOfBnds)
          wrapInPars fullApp
#else
      case (mLeftOfBnds,mRightOfBnds) of
        (Nothing,Nothing) -> error "buildSingleExpr was passed an empty list."
        ((Just leftOfBnds),Nothing) -> do
          app <- locate (GHC.OpApp leftOfBnds lROp GHC.PlaceHolder newBndStmt)
          wrapInPars app
        (Nothing, (Just rightOfBnds)) -> do
          app <- locate (GHC.OpApp newBndStmt lLOp GHC.PlaceHolder rightOfBnds)
          wrapInPars app
        ((Just leftOfBnds),(Just rightOfBnds)) -> do
          setDP (DP (0,1)) newBndStmt
          lOpApp <- locate (GHC.OpApp leftOfBnds lROp GHC.PlaceHolder newBndStmt)
          fullApp <- locate (GHC.OpApp lOpApp lLOp GHC.PlaceHolder rightOfBnds)
          wrapInPars fullApp
#endif
    mkBind :: ParsedLExpr -> RefactGhc ParsedLExpr
    mkBind e@(GHC.L _ (GHC.HsVar {})) = return e
    mkBind expr = do
      zeroDP expr
      wrapInParsWithDPs (DP (0,0)) (DP (0,0)) expr
    buildApps :: ParsedExpr -> [ParsedLExpr] -> RefactGhc (Maybe ParsedLExpr)
    buildApps op [] = return Nothing
    buildApps op [st] = return (Just st)
    buildApps op (st:stmts) = do
      mRhs <- buildApps op stmts
      case mRhs of
        Nothing -> return (Just st)
        (Just rhs) -> do
          lOp <- locate op
          addAnnVal lOp
#if __GLASGOW_HASKELL__ >= 806
          lExpr <- locate (GHC.OpApp GHC.noExt st lOp rhs)
#else
          lExpr <- locate (GHC.OpApp st lOp GHC.PlaceHolder rhs)
#endif
          return (Just lExpr)
    clusterStmts :: [GHC.ExprLStmt GhcPs] -> [[GHC.ExprLStmt GhcPs]]
    clusterStmts lst = let indices = findIndices isBindStmt lst
                           clusters = cluster indices (length lst) 0 in
                       map (\is -> map (\i -> lst !! i) is) clusters
    cluster [i] l c = [[c..(l-1)]]
    cluster (i1:i2:is) l c = let b = i1 + ((i2-i1) `div` 2) in
      [c .. b]:(cluster (i2:is) l (b+1))

{-
--Checks if a name occurs in the given ast chunk
nameOccurs :: Data a => GhcPs -> a -> Bool
nameOccurs nm = SYB.everything (||) (False `SYB.mkQ` isName)
  where isName :: GhcPs -> Bool
        isName mName = nm == mName
-}

isBindStmt :: GHC.ExprLStmt GhcPs -> Bool
isBindStmt (GHC.L _ (GHC.BindStmt {})) = True
isBindStmt _ = False

lFApp :: RefactGhc ParsedLExpr
lFApp = fApp >>= locate

fApp :: RefactGhc ParsedExpr
fApp = hsVar "<*>"


isFApp :: ParsedLExpr -> Bool
#if __GLASGOW_HASKELL__ >= 806
isFApp (GHC.L _ (GHC.HsVar _ (GHC.L _ rdrNm))) = (GHC.mkVarUnqual (fsLit "<*>")) == rdrNm
#elif __GLASGOW_HASKELL__ > 710
isFApp (GHC.L _ (GHC.HsVar (GHC.L _ rdrNm))) = (GHC.mkVarUnqual (fsLit "<*>")) == rdrNm
#else
isFApp (GHC.L _ (GHC.HsVar rdrNm)) = (GHC.mkVarUnqual (fsLit "<*>")) == rdrNm
#endif
isFApp _ = False

lLApp :: RefactGhc ParsedLExpr
lLApp = lApp >>= locate

lApp :: RefactGhc ParsedExpr
lApp = hsVar "<*"

lRApp :: RefactGhc ParsedLExpr
lRApp = rApp >>= locate

rApp :: RefactGhc ParsedExpr
rApp = hsVar "*>"

lInfixFmap :: RefactGhc ParsedLExpr
lInfixFmap = infixFmap >>= locate

infixFmap :: RefactGhc ParsedExpr
infixFmap = hsVar "<$>"

-- TODO: Move this to Utils/Variables.hs, but make it lhsVar
hsVar :: String -> RefactGhc ParsedExpr
hsVar n = do
#if __GLASGOW_HASKELL__ >= 806
  lNm <- locate $ mkRdrName n
  liftT $ addSimpleAnnT lNm (DP (0,0)) [(G GHC.AnnVal,DP (0,0))]
  return (GHC.HsVar GHC.noExt lNm)
#elif __GLASGOW_HASKELL__ > 710
  lNm <- locate $ mkRdrName n
  liftT $ addSimpleAnnT lNm (DP (0,0)) [(G GHC.AnnVal,DP (0,0))]
  return (GHC.HsVar lNm)
#else
  return (GHC.HsVar (mkRdrName n))
#endif
