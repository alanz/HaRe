{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Language.Haskell.Refact.Utils.Query where
--This module contains functions that retrieve sections of the ast. It is fairly high level.

import qualified GHC as GHC
-- import qualified SrcLoc as GHC
import qualified Id as GHC
import qualified OccName as GHC
import qualified Name as GHC
import Language.Haskell.Refact.Utils.Monad
import Language.Haskell.Refact.Utils.Synonyms
import Language.Haskell.Refact.Utils.Types
import Language.Haskell.Refact.Utils.TypeUtils
import Language.Haskell.Refact.Utils.Utils
import Data.Generics as SYB
import Control.Applicative
import FastString
import RdrName
import Language.Haskell.Refact.Utils.MonadFunctions
import qualified Data.Map as Map
import Language.Haskell.GHC.ExactPrint.Types

-- | Takes a single match and returns a tuple containing the grhs and the pattern
-- Assumptions:
--   Only a single pattern will be returned. Which pattern is returned depends
--   on the behaviour of SYB.something.
getVarAndRHS :: GHC.Match GhcPs (GHC.LHsExpr GhcPs) -> RefactGhc (GHC.LPat GhcPs, ParsedGRHSs)
getVarAndRHS match = do
  let (Just pat) = SYB.something (Nothing `SYB.mkQ` varPat) (GHC.m_pats match)
  return (pat , GHC.m_grhss match)
    where varPat lPat@(LL _ (GHC.VarPat {} )) = Just lPat
          varPat _ = Nothing

-- TODO:AZ remove this, use the API version instead
-- | Looks up the function binding at the given position. Returns nothing if the
-- position does not contain a binding.
getHsBind :: (Data ast) => SimpPos -> ast -> Maybe (GHC.HsBind GhcPs)
getHsBind pos ast =
  let rdrNm = locToRdrName pos ast in
  case rdrNm of
  Nothing -> Nothing
  (Just (GHC.L _ rNm)) -> SYB.everything (<|>) (Nothing `SYB.mkQ` isBind) ast
    where
        isBind (bnd@(GHC.FunBind { GHC.fun_id = (GHC.L _ name) } ) :: GHC.HsBind GhcPs)
            | name == rNm = (Just bnd)
        isBind _ = Nothing

-- TODO:AZ get rid of this, use the API version instead
--Get the name of a function from a string
getFunName :: (SYB.Data t) => String -> t -> Maybe GHC.Name
getFunName str = SYB.something (Nothing `SYB.mkQ` comp)
  where
    comp :: GHC.HsBind GhcRn -> Maybe GHC.Name
    comp (GHC.FunBind { GHC.fun_id = lid } )
      | (GHC.unLoc lid) `isNameString` str = Just (GHC.unLoc lid)
      | otherwise = Nothing
    comp _ = Nothing
        --This seems pretty dangerous but it'll do in a pinch
    isNameString :: GHC.Name -> String -> Bool
    isNameString nm str' = (GHC.nameOccName nm) == (GHC.mkVarOcc str')


-- TODO:AZ use a Name, not OccName, and compare properly
getTypedHsBind :: (Data a) => GHC.OccName -> a -> Maybe (GHC.HsBind GhcTc)
getTypedHsBind occ = SYB.something (Nothing `SYB.mkQ` isBind)
  where
        isBind (bnd@(GHC.FunBind { GHC.fun_id = (GHC.L _ nm) } ) :: GHC.HsBind GhcTc)
            |  (GHC.occName (GHC.idName nm)) == occ = (Just bnd)
        isBind _ = Nothing


-- TODO:AZ use Name, match on unique
-- | This looks up the type signature of the given identifier. The given
-- position is assumed to be the location of where the identifier is defined
-- NOT the location of the type signature
getTypeSig :: (Data a) => SimpPos -> String -> a -> Maybe (GHC.Sig GhcPs)
getTypeSig pos funNm a =
  let rdrNm = locToRdrName pos a in
  case rdrNm of
    Nothing -> Nothing
    (Just (GHC.L _ rNm)) -> SYB.everything (<|>) (Nothing `SYB.mkQ` isSig) a
      where
#if __GLASGOW_HASKELL__ >= 806
        isSig ty@(GHC.TypeSig _ [(GHC.L _ nm)] _) = if nm == rNm
#elif __GLASGOW_HASKELL__ > 710
        isSig ty@(GHC.TypeSig [(GHC.L _ nm)] _) = if nm == rNm
#else
        isSig ty@(GHC.TypeSig [(GHC.L _ nm)] _ _) = if nm == rNm
#endif
                                                    then (Just ty)
                                                    else Nothing
        isSig _ = Nothing

--It's common to want to know if an expression is just a certain var
--This function takes a String of the var and returns true of the given expression represents that var
isHsVar :: String -> ParsedExpr -> Bool
#if __GLASGOW_HASKELL__ >= 806
isHsVar str (GHC.HsVar _ (GHC.L _ rNm)) =
#elif __GLASGOW_HASKELL__ > 710
isHsVar str (GHC.HsVar (GHC.L _ rNm)) =
#else
isHsVar str (GHC.HsVar rNm) =
#endif
  let nm = mkVarUnqual (fsLit str) in
    rNm == nm
isHsVar _ _ = False

astCompare :: (Data a) => a -> a -> Bool
astCompare = geq `extTwinQ` rdrComp
  where rdrComp :: GHC.RdrName -> GHC.RdrName -> Bool
        rdrComp = (==)

extTwinQ :: (Typeable a, Typeable b) => (a -> a -> r) -> (b -> b -> r) -> a -> a -> r
extTwinQ f g a1 a2 =
  case mr of
    Nothing -> f a1 a2
    (Just r) -> r
  where mr = cast a1 >>= (\b1 -> cast a2 >>= (\b2 -> Just $ g b1 b2))


lookupByLoc :: (SYB.Data a,  SYB.Data b) => GHC.SrcSpan -> a -> Maybe (GHC.Located b)
lookupByLoc loc = nameSybQuery comp
  where comp :: (SYB.Data a) => GHC.Located a -> Maybe (GHC.Located a)
        comp a@(GHC.L l _)
          | l == loc = Just a
          | otherwise = Nothing
{-
lookupByLoc loc = SYB.something (Nothing `SYB.mkQ` comp)
  where comp :: (SYB.Data a) => GHC.Located a -> Maybe (GHC.Located a)
        comp a@(GHC.L l _)
          | l == loc = Just a
          | otherwise = Nothing
-}

getIdFromVar :: GHC.LHsExpr GhcPs -> RefactGhc (Maybe GHC.Id)
getIdFromVar v@(GHC.L l _var) = do
  logDataWithAnns "getIdFromVar:v=" v
  typed <- getRefactTyped
  logDataWithAnns "getIdFromVar:typed=" typed
  let (mElem :: Maybe (GHC.LHsExpr GhcTc)) = lookupByLoc l typed
  logDataWithAnns "getIdFromVar:mElem" mElem
  return $ mElem >>= (\e -> SYB.something (Nothing `SYB.mkQ` comp) e)
  where
    comp :: GHC.HsExpr GhcTc -> Maybe GHC.Id
#if __GLASGOW_HASKELL__ >= 806
    comp (GHC.HsVar _ n) = Just (GHC.unLoc n)
#elif __GLASGOW_HASKELL__ > 710
    comp (GHC.HsVar n) = Just (GHC.unLoc n)
#else
    comp (GHC.HsVar n) = Just n
#endif
    comp _ = Nothing

{-
getFunBindType :: SimpPos -> RefactGhc (Maybe GHC.Type)
getFunBindType pos = do
  typedMod <- getTypecheckedModule
  return undefined
-}

-- TODO:AZ this info should come from the AST?
-- | This checks if a syntax element is wrapped in parenthesis
-- by checking the annotatations contain AnnCloseP and AnnOpenP
isWrappedInPars :: (Data a) => (GHC.Located a) -> RefactGhc Bool
isWrappedInPars a = do
  anns <- fetchAnnsFinal
  let key = mkAnnKey a
      mAnn = Map.lookup key anns
  case mAnn of
    Nothing -> return False
    (Just ann) -> return (containsPars ann)
      where containsPars :: Annotation -> Bool
            containsPars ann' = let keywords = map fst (annsDP ann') in
              (elem (G GHC.AnnCloseP) keywords) && (elem (G GHC.AnnOpenP) keywords)
