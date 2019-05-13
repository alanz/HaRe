{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE PatternSynonyms    #-}
--------------------------------------------------------------------------------
-- Module      : Variables

-- |
--

-- ---------------------------------------------------------------------

module Language.Haskell.Refact.Utils.Variables
  (
  -- ** Variable analysis
    isFieldName
  , isClassName
  , isInstanceName
  , isDeclaredInRdr

  , FreeNames(..),DeclaredNames(..)
  , hsFreeAndDeclaredRdr
  , hsFreeAndDeclaredNameStrings
  , hsFreeAndDeclaredPNs

  , getDeclaredTypesRdr
  , getDeclaredVarsRdr

  , hsVisibleNamesRdr, hsVisibleDsRdr
  , hsFDsFromInsideRdr, hsFDNamesFromInsideRdr, hsFDNamesFromInsideRdrPure

  , rdrName2Name, rdrName2NamePure
  , eqRdrNamePure
  -- , rdrName2Name'
  , sameNameSpace

  -- ** Identifiers, expressions, patterns and declarations
  , FindEntity(..)
  , findNameInRdr
  , findNamesRdr
  , sameOccurrence
  , sameName
  , definedPNsRdr,definedNamesRdr
  , definingDeclsRdrNames,definingDeclsRdrNames',definingSigsRdrNames
  , definingTyClDeclsNames
  , definesRdr,definesDeclRdr,definesNameRdr
  , definesTypeSigRdr,definesSigDRdr

  , hsTypeVbls
  , hsNamessRdr
  , findLRdrName
  , locToName, locToNamePure
  , locToRdrName
  , locToId
  ) where

import Control.Monad.State
import Data.List
import Data.Maybe
-- import Data.Monoid

import Language.Haskell.Refact.Utils.GhcVersionSpecific
import Language.Haskell.Refact.Utils.LocUtils
import Language.Haskell.Refact.Utils.Monad
import Language.Haskell.Refact.Utils.MonadFunctions
import Language.Haskell.Refact.Utils.Types

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils


-- Modules from GHC
import qualified Bag           as GHC
import qualified GHC           as GHC
import qualified Name          as GHC
-- import qualified Outputable    as GHC
import qualified RdrName       as GHC
#if __GLASGOW_HASKELL__ >= 808
import SrcLoc (pattern LL)
#endif

import qualified Data.Generics as SYB

import qualified Data.Map as Map

import Data.Generics.Strafunski.StrategyLib.StrategyLib hiding (liftIO,MonadPlus,mzero)

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 808
#else
-- | A Pattern Synonym to Set/Get SrcSpans
pattern LL :: GHC.SrcSpan -> a -> GHC.Located a
pattern LL sp e <- GHC.L sp e
#endif

-- ---------------------------------------------------------------------

class (SYB.Data a, SYB.Typeable a) => FindEntity a where

  -- | Returns True is a syntax phrase, say a, is part of another
  -- syntax phrase, say b.
  -- NOTE: very important: only do a shallow check
  findEntity:: (SYB.Data b) => a -> b -> Bool

-- ---------------------------------------------------------------------

{-# DEPRECATED FindEntity "Can't use Renamed in GHC 8" #-}
instance FindEntity GHC.Name where

  findEntity n t = fromMaybe False res
   where
    res = SYB.something (Nothing `SYB.mkQ` worker) t

    worker (name::GHC.Name)
      | n == name = Just True
    worker _ = Nothing


-- This instance does not make sense, it will only find the specific RdrName
-- where it was found, not any other instances of it.
instance FindEntity (GHC.Located GHC.RdrName) where

  findEntity ln t =
    case SYB.something (nameSybQuery checkRdr) t of
      Nothing -> False
      _       -> True
    where
      checkRdr :: GHC.Located GHC.RdrName -> Maybe Bool
      checkRdr n
        | sameOccurrence n ln = Just True
        | otherwise = Nothing

-- ---------------------------------------------------------------------

-- TODO: should the location be matched too in this case?
instance FindEntity (GHC.Located GHC.Name) where

  findEntity n t = fromMaybe False res
   where
    res = SYB.something (Nothing `SYB.mkQ` worker) t

    worker (name::GHC.Located GHC.Name)
      | n == name = Just True
    worker _ = Nothing


-- ---------------------------------------------------------------------

instance FindEntity (GHC.LHsExpr GhcPs) where

  findEntity e t = fromMaybe False res
   where
    res = SYB.something (Nothing `SYB.mkQ` worker) t

    worker (expr :: GHC.LHsExpr GhcPs)
      | sameOccurrence e expr = Just True
    worker _ = Nothing

-- TODO: remove this instance
instance FindEntity (GHC.LHsExpr GhcRn) where

  findEntity e t = fromMaybe False res
   where
    res = SYB.something (Nothing `SYB.mkQ` worker) t

    worker (expr :: GHC.LHsExpr GhcRn)
      | sameOccurrence e expr = Just True
    worker _ = Nothing

-- ---------------------------------------------------------------------

instance FindEntity (GHC.Located (GHC.HsBindLR GhcRn GhcRn)) where
  findEntity e t = fromMaybe False res
   where
    res = SYB.something (Nothing `SYB.mkQ` worker) t

    worker (expr::(GHC.Located (GHC.HsBindLR GhcRn GhcRn)))
      | sameOccurrence e expr = Just True
    worker _ = Nothing

instance FindEntity (GHC.Located (GHC.HsDecl GhcRn)) where
  findEntity d t = fromMaybe False res
   where
    res = SYB.something (Nothing `SYB.mkQ` worker) t

    worker (decl::(GHC.Located (GHC.HsDecl GhcRn)))
      | sameOccurrence d decl = Just True
    worker _ = Nothing

-- ---------------------------------------------------------------------

-- TODO: AZ: pretty sure this can be simplified, depends if we need to
--          manage transformed stuff too though.

-- | Return True if syntax phrases t1 and t2 refer to the same one.
sameOccurrence :: (GHC.Located t) -> (GHC.Located t) -> Bool
sameOccurrence (GHC.L l1 _) (GHC.L l2 _)
 = l1 == l2

-- ---------------------------------------------------------------------

-- |Check that the given Located RdrName is the same as the given Name, by
-- looking up the equivalent name in the NameMap, and comparing Name unique
-- values.
sameName :: NameMap -> GHC.Located GHC.RdrName -> GHC.Name -> Bool
sameName nm ln name = GHC.nameUnique (rdrName2NamePure nm ln) == GHC.nameUnique name

-- ---------------------------------------------------------------------

-- | For free variables
data FreeNames = FN { fn :: [GHC.Name] }

-- | For declared variables
data DeclaredNames = DN { dn :: [GHC.Name] }

instance Show FreeNames where
  show (FN ls) = "FN " ++ showGhcQual ls

instance Show DeclaredNames where
  show (DN ls) = "DN " ++ showGhcQual ls

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup FreeNames where
  (<>) = mappend
#endif

instance Monoid FreeNames where
  mempty = FN []
  mappend (FN a) (FN b) = FN (a `mappend` b)

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup DeclaredNames where
  (<>) = mappend
#endif

instance Monoid DeclaredNames where
  mempty = DN []
  mappend (DN a) (DN b) = DN (a `mappend` b)


emptyFD :: (FreeNames,DeclaredNames)
emptyFD = (FN [], DN [])

-- ---------------------------------------------------------------------
-- | True if the name is a field name
isFieldName :: GHC.Name -> Bool
isFieldName _n = error "undefined isFieldName"

-- ---------------------------------------------------------------------
-- | True if the name is a field name
isClassName :: GHC.Name -> Bool
isClassName _n = error "undefined isClassName"

-- ---------------------------------------------------------------------
-- | True if the name is a class instance
isInstanceName :: GHC.Name -> Bool
isInstanceName _n = error "undefined isInstanceName"


-- ---------------------------------------------------------------------
-- | Collect those type variables that are declared in a given syntax phrase t. In
-- the returned result, the first list is always be empty.
hsTypeVbls::(SYB.Data t) => t -> ([GHC.RdrName],[GHC.RdrName])
hsTypeVbls =ghead "hsTypeVbls".(applyTU (stop_tdTU (failTU `adhocTU` pnt)))
  where
    -- pnt (PNT (PN i (UniqueNames.S loc)) (Type _) _) = return ([], [(PN i (UniqueNames.S loc))])
    pnt n | GHC.rdrNameSpace n == GHC.tvName = return ([], [n])
    pnt _ = mzero


-------------------------------------------------------------------------------

isDeclaredInRdr :: NameMap -> GHC.Name -> [GHC.LHsDecl GhcPs] -> Bool
isDeclaredInRdr nm name decls = nonEmptyList $ definingDeclsRdrNames nm [name] decls False True

-- ---------------------------------------------------------------------

-- |The same as `hsFreeAndDeclaredPNs` except that the returned
-- variables are in the String format.
hsFreeAndDeclaredNameStrings :: (SYB.Data t)
  => t -> RefactGhc ([String],[String])
hsFreeAndDeclaredNameStrings t = do
  (f,d) <- hsFreeAndDeclaredPNs t
  return ((nub.map showGhc) f, (nub.map showGhc) d)

-- ---------------------------------------------------------------------

-- | Return the free and declared Names in the given syntax fragment. The syntax
-- fragment MUST be parameterised by RdrName, else the empty list will be
-- returned.
hsFreeAndDeclaredPNs :: (SYB.Data t) => t -> RefactGhc ([GHC.Name],[GHC.Name])
hsFreeAndDeclaredPNs t = do
  nm <- getRefactNameMap
  -- TODO: Change the return type to (FreeNames,DeclaredNames)
  let (FN f,DN d) = hsFreeAndDeclaredRdr nm t
  return (f,d)

-- ---------------------------------------------------------------------

-- | Collect the free and declared variables (in the GHC.Name format)
-- in a given syntax phrase t. In the result, the first list contains
-- the free variables, and the second list contains the declared
-- variables.
-- Expects ParsedSource
hsFreeAndDeclaredRdr :: (SYB.Data t) => NameMap -> t -> (FreeNames,DeclaredNames)
hsFreeAndDeclaredRdr nm t = res
  where
    fd = hsFreeAndDeclaredRdr' nm t
    (FN f,DN d) = case fd of
      Left _err -> mempty
      Right v -> v
    res = (FN (f \\ d),DN d)

hsFreeAndDeclaredRdr':: (SYB.Data t) => NameMap -> t -> Either String (FreeNames,DeclaredNames)
hsFreeAndDeclaredRdr' nm t = do
      (FN f,DN d) <- hsFreeAndDeclared'
      -- let (f',d') = ( filter (not . GHC.isTyVarName) $ nub f
      --               , filter (not . GHC.isTyVarName) $ nub d)
      let (f',d') = ( nub f
                    , nub d)
      return (FN f',DN d')

   where
          hsFreeAndDeclared' :: Either String (FreeNames,DeclaredNames)
          hsFreeAndDeclared'  = applyTU (stop_tdTU (failTU
                                                      `adhocTU` expr
                                                      `adhocTU` pat
                                                      `adhocTU` bndrs
                                                      `adhocTU` binds
                                                      `adhocTU` bindList
                                                      `adhocTU` match
                                                      `adhocTU` stmtlist
                                                      `adhocTU` stmts
                                                      `adhocTU` rhs
                                                      `adhocTU` ltydecl
                                                      `adhocTU` tyvarbndrs
                                                      `adhocTU` lhstyvarbndr
#if __GLASGOW_HASKELL__ > 710
                                                      `adhocTU` lsigtype
#endif
                                                      `adhocTU` sig
                                                      `adhocTU` datadefn
                                                      `adhocTU` condecl
                                                      `adhocTU` condetails
                                                      `adhocTU` condeclfield
                                                      `adhocTU` hstype
                                                       )) t


          -- expr --
#if __GLASGOW_HASKELL__ >= 806
          expr (GHC.L l (GHC.HsVar _ (GHC.L _ n)))
#elif __GLASGOW_HASKELL__ > 710
          expr (GHC.L l (GHC.HsVar (GHC.L _ n)))
#else
          expr (GHC.L l (GHC.HsVar n))
#endif
            = return (FN [rdrName2NamePure nm (GHC.L l n)],DN [])

#if __GLASGOW_HASKELL__ >= 806
          expr (GHC.L _ (GHC.OpApp _ e1 (GHC.L l (GHC.HsVar _ (GHC.L _ n))) e2)) = do
#elif __GLASGOW_HASKELL__ > 710
          expr (GHC.L _ (GHC.OpApp e1 (GHC.L l (GHC.HsVar (GHC.L _ n))) _ e2)) = do
#else
          expr (GHC.L _ (GHC.OpApp e1 (GHC.L l (GHC.HsVar n)) _ e2)) = do
#endif
              efed <- hsFreeAndDeclaredRdr' nm [e1,e2]
              fd   <- addFree (rdrName2NamePure nm (GHC.L l n)) efed
              return fd

#if __GLASGOW_HASKELL__ >= 806
          expr (GHC.L _ ((GHC.HsLam _ (GHC.MG _ matches _))) :: GHC.LHsExpr GhcPs) =
#else
          expr (GHC.L _ ((GHC.HsLam (GHC.MG matches _ _ _))) :: GHC.LHsExpr GhcPs) =
#endif
             hsFreeAndDeclaredRdr' nm matches

#if __GLASGOW_HASKELL__ >= 806
          expr (GHC.L _ ((GHC.HsLet _ decls e)) :: GHC.LHsExpr GhcPs) =
#else
          expr (GHC.L _ ((GHC.HsLet decls e)) :: GHC.LHsExpr GhcPs) =
#endif
            do
              (FN df,DN dd) <- hsFreeAndDeclaredRdr' nm decls
              (FN ef,_)  <- hsFreeAndDeclaredRdr' nm e
              return (FN (df `union` (ef \\ dd)),DN [])

#if __GLASGOW_HASKELL__ >= 806
          expr (GHC.L _ (GHC.RecordCon _ ln e)) = do
#elif __GLASGOW_HASKELL__ > 710
          expr (GHC.L _ (GHC.RecordCon ln _ _ e)) = do
#else
          expr (GHC.L _ (GHC.RecordCon ln _ e)) = do
#endif
            fd <- hsFreeAndDeclaredRdr' nm e
            addFree (rdrName2NamePure nm ln) fd   --Need Testing

#if __GLASGOW_HASKELL__ >= 806
          expr (GHC.L _ (GHC.EAsPat _ ln e)) = do
#else
          expr (GHC.L _ (GHC.EAsPat ln e)) = do
#endif
            fd <- (hsFreeAndDeclaredRdr' nm e)
            addFree (rdrName2NamePure nm ln) fd

          expr _ = mzero


          -- rhs --
#if __GLASGOW_HASKELL__ >= 806
          rhs ((GHC.GRHSs _ g ds) :: GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs))
#else
          rhs ((GHC.GRHSs g ds) :: GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs))
#endif
            = do (FN df,DN dd) <- hsFreeAndDeclaredRdr' nm g
                 (FN ef,DN ed) <- hsFreeAndDeclaredRdr' nm ds
                 return (FN $ df ++ ef, DN $ dd ++ ed)

#if __GLASGOW_HASKELL__ >= 806
          rhs (GHC.XGRHSs _ ) = return (FN [], DN [])
#endif


          -- pat --
          pat :: GHC.LPat GhcPs -> Either String (FreeNames,DeclaredNames)
          -- pat (GHC.L _ (GHC.WildPat _)) = mzero
          pat (LL _ (GHC.WildPat _)) = mzero
#if __GLASGOW_HASKELL__ >= 806
          pat (LL l (GHC.VarPat _ (GHC.L _ n)))
#elif __GLASGOW_HASKELL__ > 710
          pat (GHC.L l (GHC.VarPat (GHC.L _ n)))
#else
          pat (GHC.L l (GHC.VarPat n))
#endif
            = return (FN [],DN [rdrName2NamePure nm (GHC.L l n)])
#if __GLASGOW_HASKELL__ >= 806
          pat (LL _ (GHC.AsPat _ ln p)) = do
#else
          pat (GHC.L _ (GHC.AsPat ln p)) = do
#endif
            (f,DN d) <- hsFreeAndDeclaredRdr' nm p
            return (f,DN (rdrName2NamePure nm ln:d))

#if __GLASGOW_HASKELL__ >= 806
          pat (LL _ (GHC.ParPat _ p)) = pat p
          pat (LL _ (GHC.BangPat _ p)) = pat p
#else
          pat (GHC.L _ (GHC.ParPat p)) = pat p
          pat (GHC.L _ (GHC.BangPat p)) = pat p
#endif
#if __GLASGOW_HASKELL__ >= 806
          pat (LL _ (GHC.ListPat _ ps)) = do
#else
          pat (GHC.L _ (GHC.ListPat ps _ _)) = do
#endif
            fds <- mapM pat ps
            return $ mconcat fds
#if __GLASGOW_HASKELL__ >= 806
          pat (LL _ (GHC.TuplePat _ ps _)) = do
#else
          pat (GHC.L _ (GHC.TuplePat ps _ _)) = do
#endif
            fds <- mapM pat ps
            return $ mconcat fds
#if __GLASGOW_HASKELL__ < 806
          pat (GHC.L _ (GHC.PArrPat ps _)) = do
            fds <- mapM pat ps
            return $ mconcat fds
#endif
          pat (LL _ (GHC.ConPatIn n det)) = do
            (FN f,DN d) <- details det
            return $ (FN [rdrName2NamePure nm n],DN d) <> (FN [],DN f)
#if __GLASGOW_HASKELL__ >= 806
          pat (LL _ (GHC.ViewPat _ e p)) = do
#else
          pat (GHC.L _ (GHC.ViewPat e p _)) = do
#endif
            fde <- hsFreeAndDeclaredRdr' nm e
            fdp <- pat p
            return $ fde <> fdp
          pat (LL _ (GHC.LitPat {})) = return emptyFD
#if __GLASGOW_HASKELL__ >= 806
          pat (LL _ (GHC.NPat {})) = return emptyFD
          pat (LL _ (GHC.NPlusKPat _ n _ _ _ _)) = return (FN [],DN [rdrName2NamePure nm n])
#elif __GLASGOW_HASKELL__ > 710
          pat (GHC.L _ (GHC.NPat _ _ _ _)) = return emptyFD
          pat (GHC.L _ (GHC.NPlusKPat n _ _ _ _ _)) = return (FN [],DN [rdrName2NamePure nm n])
#else
          pat (GHC.L _ (GHC.NPat _ _ _)) = return emptyFD
          pat (GHC.L _ (GHC.NPlusKPat n _ _ _)) = return (FN [],DN [rdrName2NamePure nm n])
#endif
#if __GLASGOW_HASKELL__ >= 808
          pat (LL _ (GHC.SigPat _ p b)) = do
#elif __GLASGOW_HASKELL__ >= 806
          pat (GHC.L _ (GHC.SigPat b p)) = do
#else
          pat (GHC.L _ (GHC.SigPatIn p b)) = do
#endif
            fdp <- pat p
            (FN fb,DN _db) <- hsFreeAndDeclaredRdr' nm b
            -- error $ "pat.SigPatIn:(b,fb,db)" ++ showGhc (b,fb,db)
            return $ fdp <> (FN fb,DN [])
#if __GLASGOW_HASKELL__ < 806
          pat (GHC.L _ (GHC.SigPatOut p _)) = pat p
#endif
#if __GLASGOW_HASKELL__ >= 806
          pat (LL l (GHC.CoPat _ _ p _)) = pat (LL l p)
#else
          pat (GHC.L l (GHC.CoPat _ p _)) = pat (GHC.L l p)
#endif

#if __GLASGOW_HASKELL__ >= 806
          pat (LL _ (GHC.LazyPat _ p)) = pat p
#else
          pat (GHC.L _ (GHC.LazyPat p)) = pat p
#endif

          pat (LL _ (GHC.ConPatOut {})) = error $ "hsFreeAndDeclaredRdr'.pat:impossible: ConPatOut"

#if __GLASGOW_HASKELL__ >= 806
          pat (LL _ (GHC.SplicePat _ (GHC.HsQuasiQuote {})))         = return (FN [], DN [])
          pat (LL _ (GHC.SplicePat _ (GHC.HsUntypedSplice _ _ _ e))) = hsFreeAndDeclaredRdr' nm e
          pat (LL _ (GHC.SplicePat _ (GHC.HsTypedSplice _ _ _ e)))   = hsFreeAndDeclaredRdr' nm e
          pat (LL _ (GHC.SplicePat _ (GHC.HsSpliced {})))            = error $ "hsFreeAndDeclaredRdr'.pat:impossible: HsSpliced"
          pat (LL _ (GHC.SplicePat _ (GHC.XSplice {})))              = mzero
#elif __GLASGOW_HASKELL__ > 800
          pat (LL _ (GHC.SplicePat (GHC.HsQuasiQuote {})))     = return (FN [], DN [])
          pat (LL _ (GHC.SplicePat (GHC.HsUntypedSplice _ _ e))) = hsFreeAndDeclaredRdr' nm e
          pat (LL _ (GHC.SplicePat (GHC.HsTypedSplice _ _ e)))   = hsFreeAndDeclaredRdr' nm e
          pat (LL _ (GHC.SplicePat (GHC.HsSpliced _ _))) = error $ "hsFreeAndDeclaredRdr'.pat:impossible: HsSpliced"
#elif __GLASGOW_HASKELL__ > 710
          pat (GHC.L _ (GHC.SplicePat (GHC.HsQuasiQuote {})))     = return (FN [], DN [])
          pat (GHC.L _ (GHC.SplicePat (GHC.HsTypedSplice _ e)))   = hsFreeAndDeclaredRdr' nm e
#else
          pat (GHC.L _ (GHC.SplicePat (GHC.HsSplice _ e))) = hsFreeAndDeclaredRdr' nm e
#endif

{-
#if __GLASGOW_HASKELL__ <= 710
          pat (GHC.L _ (GHC.SplicePat (GHC.HsSplice _ e))) = hsFreeAndDeclaredRdr' nm e
#else
          pat (GHC.L _ (GHC.SplicePat (GHC.HsQuasiQuote {})))     = return (FN [], DN [])
#  if __GLASGOW_HASKELL__ <= 800
          pat (GHC.L _ (GHC.SplicePat (GHC.HsTypedSplice _ e)))   = hsFreeAndDeclaredRdr' nm e
#  else
          pat (GHC.L _ (GHC.SplicePat (GHC.HsUntypedSplice _ _ e))) = hsFreeAndDeclaredRdr' nm e
          pat (GHC.L _ (GHC.SplicePat (GHC.HsTypedSplice _ _ e)))   = hsFreeAndDeclaredRdr' nm e
          pat (GHC.L _ (GHC.SplicePat (GHC.HsSpliced _ _))) = error $ "hsFreeAndDeclaredRdr'.pat:impossible: HsSpliced"
#  endif
#endif
-}

#if __GLASGOW_HASKELL__ <= 710
          pat (GHC.L _ (GHC.QuasiQuotePat _)) = return (FN [], DN [])
#endif

#if __GLASGOW_HASKELL__ > 800
          pat (LL _ (GHC.SumPat p _ _ _)) = hsFreeAndDeclaredRdr' nm p
#endif

#if __GLASGOW_HASKELL__ >= 806
          pat (LL _ (GHC.XPat _)) = mzero
#endif
          -- pat p = error $ "hsFreeAndDeclaredRdr'.pat:unimplemented:" ++ (showGhc p)

          -- ---------------------------

          details :: GHC.HsConPatDetails GhcPs -> Either String (FreeNames,DeclaredNames)
          details (GHC.PrefixCon  args) = do
            fds <- mapM pat args
            return $ mconcat fds
          details (GHC.RecCon recf) =
            recfields recf
          details (GHC.InfixCon arg1 arg2) = do
            fds <- mapM pat [arg1,arg2]
            return $ mconcat fds

          -- Note: this one applies to HsRecFields in LPats
          recfields :: (GHC.HsRecFields GhcPs (GHC.LPat GhcPs)) -> Either String (FreeNames,DeclaredNames)
          recfields (GHC.HsRecFields fields _) = do
            let args = map (\(GHC.L _ (GHC.HsRecField _ arg _)) -> arg) fields
            fds <- mapM pat args
            return $ mconcat fds

          -- -----------------------

#if __GLASGOW_HASKELL__ <= 710
          bndrs :: GHC.HsWithBndrs GhcPs (GHC.LHsType GhcPs) -> Either String (FreeNames,DeclaredNames)
          bndrs (GHC.HsWB thing _ _ _) = do
            (FN ft,DN _dt) <- hsFreeAndDeclaredRdr' nm thing
            return (FN ft,DN [])
#else
          bndrs :: GHC.LHsSigWcType GhcPs -> Either String (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
          bndrs (GHC.HsWC _ (GHC.HsIB _ ty)) = do
#elif __GLASGOW_HASKELL__ > 800
          bndrs (GHC.HsWC _ (GHC.HsIB _ ty _)) = do
#else
          bndrs (GHC.HsIB _ (GHC.HsWC _ _ ty)) = do
#endif
            (FN ft,DN _dt) <- hsFreeAndDeclaredRdr' nm ty
            -- return (FN dt,DN [])
            return (FN ft,DN [])
#endif
#if __GLASGOW_HASKELL__ >= 806
          bndrs (GHC.XHsWildCardBndrs _) = mzero
          bndrs (GHC.HsWC  _ (GHC.XHsImplicitBndrs _)) = mzero
#endif

          -- ---------------------------

          bindList (ds :: [GHC.LHsBind GhcPs])
            =do (FN f,DN d) <- recurseList ds
                return (FN (f\\d),DN d)

          -- match and patBind, same type--
#if __GLASGOW_HASKELL__ >= 806
          binds ((GHC.FunBind _ ln (GHC.MG _ matches _) _ _) :: GHC.HsBind GhcPs)
#elif __GLASGOW_HASKELL__ > 710
          binds ((GHC.FunBind ln (GHC.MG matches _ _ _) _ _fvs _) :: GHC.HsBind GhcPs)
#else
          binds ((GHC.FunBind ln _ (GHC.MG matches _ _ _) _ _fvs _) :: GHC.HsBind GhcPs)
#endif
            = do
                (FN pf,_pd) <- hsFreeAndDeclaredRdr' nm matches
                let n = rdrName2NamePure nm ln
                return (FN (pf \\ [n]) ,DN [n])

          -- patBind --
#if __GLASGOW_HASKELL__ >= 806
          binds (GHC.PatBind _ pat' prhs _) =
#else
          binds (GHC.PatBind pat' prhs _ _ds _) =
#endif
            do
              (FN pf,DN pd) <- hsFreeAndDeclaredRdr' nm pat'
              (FN rf,DN rd) <- hsFreeAndDeclaredRdr' nm prhs
              return (FN $ pf `union` (rf \\ pd),DN $ pd ++ rd)

          binds _ = mzero

#if __GLASGOW_HASKELL__ >= 806
          match ((GHC.Match _ _fn pats mrhs) :: GHC.Match GhcPs (GHC.LHsExpr GhcPs))
#elif __GLASGOW_HASKELL__ >= 804
          match ((GHC.Match _fn pats mrhs) :: GHC.Match GhcPs (GHC.LHsExpr GhcPs))
#else
          match ((GHC.Match _fn pats _mtype mrhs) :: GHC.Match GhcPs (GHC.LHsExpr GhcPs))
#endif
            = do
              (FN pf,DN pd) <- hsFreeAndDeclaredRdr' nm pats
              (FN rf,DN rd) <- hsFreeAndDeclaredRdr' nm mrhs
              return (FN (pf `union` (rf \\ (pd `union` rd))),DN [])
#if __GLASGOW_HASKELL__ >= 806
          match (GHC.XMatch _) = mzero
#endif

          stmtlist (ds :: [GHC.ExprLStmt GhcPs]) = do
            (FN f,DN d) <- recurseList ds
            -- unless (null ds) $ do
            --   -- error $ "hsFreeAndDeclaredRdr'.stmtlist ds=" ++ showGhc ds
            --   error $ "hsFreeAndDeclaredRdr'.stmtlist (f,d)=" ++ showGhc (f,d)
            return (FN (f\\d),DN d)

          -- stmts --
#if __GLASGOW_HASKELL__ >= 806
          stmts ((GHC.BindStmt _x pat' expre _bindOp _failOp) :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) = do
#elif __GLASGOW_HASKELL__ > 710
          stmts ((GHC.BindStmt pat' expre _bindOp _failOp _) :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) = do
#else
          stmts ((GHC.BindStmt pat' expre _bindOp _failOp) :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) = do
#endif
            -- TODO ++AZ++ : Not sure it is meaningful to pull
            --               anything out of bindOp/failOp
            (FN pf,DN pd)  <- hsFreeAndDeclaredRdr' nm pat'
            -- error $ "hsFreeAndDeclaredRdr'.stmts.BindStmt (pf,pd)=" ++ showGhc (pf,pd)
            (FN ef,_ed) <- hsFreeAndDeclaredRdr' nm expre
            let sf1 = []
            return (FN $ pf `union` ef `union` (sf1\\pd),DN pd) -- pd) -- Check this

#if __GLASGOW_HASKELL__ >= 806
          stmts ((GHC.LetStmt _ binds') :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) =
#else
          stmts ((GHC.LetStmt binds') :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) =
#endif
            hsFreeAndDeclaredRdr' nm binds'

          stmts _ = mzero

          -- tycldecls -----------------

          ltydecl :: GHC.TyClDecl GhcPs -> Either String (FreeNames,DeclaredNames)

#if __GLASGOW_HASKELL__ >= 806
          ltydecl (GHC.FamDecl _ fd) = hsFreeAndDeclaredRdr' nm fd
#else
          ltydecl (GHC.FamDecl fd) = hsFreeAndDeclaredRdr' nm fd
#endif
#if __GLASGOW_HASKELL__ >= 806
          ltydecl (GHC.SynDecl _ ln _bndrs _ _)
#elif __GLASGOW_HASKELL__ > 800
          ltydecl (GHC.SynDecl ln _bndrs _ _ _fvs)
#else
          ltydecl (GHC.SynDecl ln _bndrs _rhs _fvs)
#endif
              = return (FN [],DN [rdrName2NamePure nm ln])
#if __GLASGOW_HASKELL__ >= 806
          ltydecl (GHC.DataDecl _ ln tyvars _f defn) = do
#elif __GLASGOW_HASKELL__ > 800
          ltydecl (GHC.DataDecl ln tyvars _f defn _c _fvs) = do
#elif __GLASGOW_HASKELL__ > 710
          ltydecl (GHC.DataDecl ln tyvars defn _c _fvs) = do
#else
          ltydecl (GHC.DataDecl ln tyvars defn _fvs) = do
#endif
              (FN fs,DN dds) <- hsFreeAndDeclaredRdr' nm  defn
              (FN _ft,DN dt) <- hsFreeAndDeclaredRdr' nm  tyvars
              return (FN (fs \\ dt),DN (rdrName2NamePure nm ln:dds))
#if __GLASGOW_HASKELL__ >= 806
          ltydecl (GHC.ClassDecl _ ctx ln tyvars _fixity
                           _fds sigs meths ats atds _docs) = do
#elif __GLASGOW_HASKELL__ > 800
          ltydecl (GHC.ClassDecl ctx ln tyvars _fixity
                           _fds sigs meths ats atds _docs _fvs) = do
#else
          ltydecl (GHC.ClassDecl ctx ln tyvars
                           _fds sigs meths ats atds _docs _fvs) = do
#endif
             ct  <- hsFreeAndDeclaredRdr' nm ctx
             (_,DN tv)  <- hsFreeAndDeclaredRdr' nm tyvars
             ss  <- recurseList sigs
             md  <- hsFreeAndDeclaredRdr' nm meths
             ad  <- hsFreeAndDeclaredRdr' nm ats
             atd <- hsFreeAndDeclaredRdr' nm atds
             let (FN ff,DN df) = ((FN [],DN [rdrName2NamePure nm ln]) <> md <> ad <> atd <> ct <> ss)
             return (FN (nub ff \\ tv), DN df)
#if __GLASGOW_HASKELL__ >= 806
          ltydecl (GHC.XTyClDecl _) = mzero
#endif

          ------------------------------

#if __GLASGOW_HASKELL__ >= 806
          tyvarbndrs :: GHC.LHsQTyVars GhcPs -> Either String (FreeNames,DeclaredNames)
          tyvarbndrs (GHC.HsQTvs _ explicit ) = recurseList explicit
          tyvarbndrs (GHC.XLHsQTyVars _)      = mzero
#elif __GLASGOW_HASKELL__ > 710
          tyvarbndrs :: GHC.LHsQTyVars GhcPs -> Either String (FreeNames,DeclaredNames)
          tyvarbndrs (GHC.HsQTvs _implicit explicit _dependent ) = recurseList explicit
#else
          tyvarbndrs :: GHC.LHsTyVarBndrs GhcPs -> Either String (FreeNames,DeclaredNames)
          tyvarbndrs (GHC.HsQTvs _implicit explicit) = do
            recurseList explicit
#endif

          lhstyvarbndr :: GHC.LHsTyVarBndr GhcPs -> Either String (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
          lhstyvarbndr (GHC.L _ (GHC.UserTyVar _ ln)) = return (FN [], DN [rdrName2NamePure nm ln])
          lhstyvarbndr (GHC.L _ (GHC.XTyVarBndr _)) = mzero
#elif __GLASGOW_HASKELL__ > 710
          lhstyvarbndr (GHC.L _ (GHC.UserTyVar ln)) = return (FN [], DN [rdrName2NamePure nm ln])
#else
          lhstyvarbndr (GHC.L l (GHC.UserTyVar n)) = return (FN [], DN [rdrName2NamePure nm (GHC.L l n)])
#endif
#if __GLASGOW_HASKELL__ >= 806
          lhstyvarbndr (GHC.L _ (GHC.KindedTyVar _ ln lk)) = do
#else
          lhstyvarbndr (GHC.L _ (GHC.KindedTyVar ln lk)) = do
#endif
            ks <- hsFreeAndDeclaredRdr' nm lk
            return ((FN [], DN [rdrName2NamePure nm ln]) <> ks)

          ------------------------------

#if __GLASGOW_HASKELL__ > 710
          lsigtype :: GHC.LHsSigType GhcPs -> Either String (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
          lsigtype (GHC.HsIB _ typ) = do
#elif __GLASGOW_HASKELL__ > 800
          lsigtype (GHC.HsIB _ typ _) = do
#else
          lsigtype (GHC.HsIB _ typ) = do
#endif
            hsFreeAndDeclaredRdr' nm typ
#endif
#if __GLASGOW_HASKELL__ >= 806
          lsigtype (GHC.XHsImplicitBndrs _) = mzero
#endif

          ------------------------------

          sig :: GHC.Sig GhcPs -> Either String (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
          sig (GHC.TypeSig _ lns typ) = do
#elif __GLASGOW_HASKELL__ > 710
          sig (GHC.TypeSig lns typ) = do
#else
          sig (GHC.TypeSig lns typ _) = do
#endif
            (FN ft, dt) <- hsFreeAndDeclaredRdr' nm typ
            -- error $ "sig:ft=" ++ (intercalate "," $ map (\n -> showGhc n ++ (occAttributes $ GHC.occName n)) ft)
            return ((FN [],DN (map (rdrName2NamePure nm ) lns))
                     <> (FN (filter (not . GHC.isTyVarName) ft), dt))
#if __GLASGOW_HASKELL__ >= 806
          sig (GHC.PatSynSig _ lns typ) = do
            ts <- hsFreeAndDeclaredRdr' nm typ
            return ((FN [],DN (map (rdrName2NamePure nm) lns)) <> ts)
#elif __GLASGOW_HASKELL__ > 800
          sig (GHC.PatSynSig lns typ) = do
            ts <- hsFreeAndDeclaredRdr' nm typ
            return ((FN [],DN (map (rdrName2NamePure nm) lns)) <> ts)
#elif __GLASGOW_HASKELL__ > 710
          sig (GHC.PatSynSig ln typ) = do
            ts <- hsFreeAndDeclaredRdr' nm typ
            return ((FN [],DN [rdrName2NamePure nm ln]) <> ts)
#else
          sig (GHC.PatSynSig ln (_ef,GHC.HsQTvs _ns bndrs) ctx1 ctx2 typ) = do
            (_, DN bs) <- hsFreeAndDeclaredRdr' nm bndrs
            c1s <- hsFreeAndDeclaredRdr' nm ctx1
            c2s <- hsFreeAndDeclaredRdr' nm ctx2
            ts  <- hsFreeAndDeclaredRdr' nm typ
            let (FN f,DN d) = c1s <> c2s <> ts
                fd = (FN (f \\ bs), DN d )
            return ((FN [],DN [rdrName2NamePure nm ln]) <> fd)
#endif

#if __GLASGOW_HASKELL__ >= 806
          sig (GHC.ClassOpSig _ _ lns typ) = do
#elif __GLASGOW_HASKELL__ > 710
          sig (GHC.ClassOpSig _ lns typ) = do
#else
          sig (GHC.GenericSig lns typ) = do
#endif
            ts <- hsFreeAndDeclaredRdr' nm typ
            return ((FN [],DN (map (rdrName2NamePure nm) lns)) <> ts)
          sig (GHC.IdSig {}) = error $ "hsFreeAndDeclaredRdr:IdSig should not occur"
#if __GLASGOW_HASKELL__ >= 806
          sig (GHC.FixSig _ fsig) = hsFreeAndDeclaredRdr' nm fsig
#else
          sig (GHC.FixSig fsig) = hsFreeAndDeclaredRdr' nm fsig
#endif
#if __GLASGOW_HASKELL__ >= 806
          sig (GHC.InlineSig _ ln _) = do
#else
          sig (GHC.InlineSig ln _) = do
#endif
            return ((FN [],DN [rdrName2NamePure nm ln]) )
#if __GLASGOW_HASKELL__ >= 806
          sig (GHC.SpecSig _ ln typs _) = do
#else
          sig (GHC.SpecSig ln typs _) = do
#endif
            ts <- recurseList typs
            return ((FN [rdrName2NamePure nm ln],DN []) <> ts)
#if __GLASGOW_HASKELL__ >= 806
          sig (GHC.SpecInstSig _ _ ssig) = hsFreeAndDeclaredRdr' nm ssig
#else
          sig (GHC.SpecInstSig _ ssig) = hsFreeAndDeclaredRdr' nm ssig
#endif
          sig (GHC.MinimalSig {}) = return mempty
#if __GLASGOW_HASKELL__ > 800
          sig (GHC.SCCFunSig {})        = return mempty
          sig (GHC.CompleteMatchSig {}) = return mempty
#endif
#if __GLASGOW_HASKELL__ >= 806
          sig (GHC.XSig _) = mzero
#endif

          ------------------------------

          datadefn :: GHC.HsDataDefn GhcPs -> Either String (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
          datadefn (GHC.HsDataDefn _ _ ctxt mtyp mkind cons mderivs) = do
#else
          datadefn (GHC.HsDataDefn _ ctxt mtyp mkind cons mderivs) = do
#endif
             cts <- mapM (hsFreeAndDeclaredRdr' nm) $ GHC.unLoc ctxt
             ts  <- maybeHelper mtyp
             ks  <- maybeHelper mkind
             cs  <- mapM (hsFreeAndDeclaredRdr' nm) cons
#if __GLASGOW_HASKELL__ <= 800
             ds  <- case mderivs of
                      Nothing           -> return (FN [],DN [])
                      Just (GHC.L _ ds) -> recurseList ds
#else
             ds  <- case mderivs of
                      (GHC.L _ ds) -> recurseList ds
#endif
             return $ mconcat [mconcat cts,ts,ks,mconcat cs,ds]
#if __GLASGOW_HASKELL__ >= 806
          datadefn (GHC.XHsDataDefn _) = mzero
#endif
             -- error $ "hit datadefn:[cts,ts,ks,cs,ds]=" ++ show (cts++ [ts,ks,ds] ++ cs)

          ------------------------------

          condecl :: GHC.LConDecl GhcPs -> Either String (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
          condecl (GHC.L _ (GHC.ConDeclGADT _ ns _ _qvars mctxt dets typ _)) = do
            cs <- maybeHelper mctxt
            ds <- hsFreeAndDeclaredRdr' nm dets
            (ft,_) <- hsFreeAndDeclaredRdr' nm typ
            return ((ft,DN (map (rdrName2NamePure nm) ns)) <> cs <> ds)
{-
    = ConDeclGADT
      { con_g_ext   :: XConDeclGADT pass
      , con_names   :: [Located (IdP pass)]

      -- The next four fields describe the type after the '::'
      -- See Note [GADT abstract syntax]
      -- The following field is Located to anchor API Annotations,
      -- AnnForall and AnnDot.
      , con_forall  :: Located Bool      -- ^ True <=> explicit forall
                                         --   False => hsq_explicit is empty
      , con_qvars   :: LHsQTyVars pass
                       -- Whether or not there is an /explicit/ forall, we still
                       -- need to capture the implicitly-bound type/kind variables

      , con_mb_cxt  :: Maybe (LHsContext pass) -- ^ User-written context (if any)
      , con_args    :: HsConDeclDetails pass   -- ^ Arguments; never InfixCon
      , con_res_ty  :: LHsType pass            -- ^ Result type

      , con_doc     :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }
-}
          condecl (GHC.L _ (GHC.ConDeclH98 _ n _ _ mctxt dets _)) = do
             cs <- maybeHelper mctxt
             ds <- hsFreeAndDeclaredRdr' nm dets
             return ((FN [], DN ([rdrName2NamePure nm n])) <> cs <> ds)
          condecl (GHC.L _ (GHC.XConDecl _)) = mzero
#elif __GLASGOW_HASKELL__ > 710
          condecl (GHC.L _ (GHC.ConDeclGADT ns typ _)) = do
            (ft,_) <- hsFreeAndDeclaredRdr' nm typ
            return (ft,DN (map (rdrName2NamePure nm) ns))
          condecl (GHC.L _ (GHC.ConDeclH98 n _ mctxt dets _)) = do
             cs <- maybeHelper mctxt
             ds <- hsFreeAndDeclaredRdr' nm dets
             return ((FN [], DN ([rdrName2NamePure nm n])) <> cs <> ds)
#else
          condecl (GHC.L _ (GHC.ConDecl ns _expr (GHC.HsQTvs _ns bndrs) ctxt
                                        dets res _ depc_syntax)) =
            case res of
              GHC.ResTyGADT ls typ -> do
                (ft,_) <- hsFreeAndDeclaredRdr' nm typ
                return (ft,DN (map (rdrName2NamePure nm) ns))
              GHC.ResTyH98 -> do
                cs <- hsFreeAndDeclaredRdr' nm ctxt
                ds <- hsFreeAndDeclaredRdr' nm dets
                return ((FN [], DN (map (rdrName2NamePure nm) ns)) <> cs <> ds)
#endif

          ------------------------------

          -- condetails :: GHC.HsConDetails (GHC.LBangType GHC.RdrName) (GHC.Located [GHC.LConDeclField GHC.RdrName])
          condetails :: GHC.HsConDeclDetails GhcPs -> Either String (FreeNames,DeclaredNames)
          condetails (GHC.PrefixCon args)      = do
            -- TODO: get rid of the tyvars
            (FN fs,d) <- recurseList args
            return (FN (filter (not . GHC.isTyVarName) fs),d)
          condetails (GHC.RecCon (GHC.L _ fs)) = recurseList fs
          condetails (GHC.InfixCon a1 a2)      = do
            -- TODO: get rid of the tyvars
            (FN fs,d) <- recurseList [a1,a2]
            return (FN (filter (not . GHC.isTyVarName) fs),d)

          ------------------------------

          condeclfield :: GHC.LConDeclField GhcPs -> Either String (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
          condeclfield (GHC.L _ (GHC.ConDeclField _ fns typ _)) = do
#else
          condeclfield (GHC.L _ (GHC.ConDeclField fns typ _)) = do
#endif
#if __GLASGOW_HASKELL__ <= 710
            let ns = fns
#else
            let ns = map (GHC.rdrNameFieldOcc . GHC.unLoc) fns
#endif
            dt <- hsFreeAndDeclaredRdr' nm typ
            return ((FN [],DN (map (rdrName2NamePure nm) ns)) <> dt)
#if __GLASGOW_HASKELL__ >= 806
          condeclfield (GHC.L _ (GHC.XConDeclField _)) = mzero
#endif

          ------------------------------

          hstype :: GHC.LHsType GhcPs -> Either String (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
          hstype (GHC.L _ (GHC.HsForAllTy _ _ typ)) = hsFreeAndDeclaredRdr' nm typ
#elif __GLASGOW_HASKELL__ > 710
          hstype (GHC.L _ (GHC.HsForAllTy _ typ)) = hsFreeAndDeclaredRdr' nm typ
#else
          hstype (GHC.L _ (GHC.HsForAllTy _ _ _ _ typ)) = hsFreeAndDeclaredRdr' nm typ
#endif
#if __GLASGOW_HASKELL__ >= 806
          hstype (GHC.L _ (GHC.HsTyVar _ _ n)) = return (FN [rdrName2NamePure nm n],DN [])
#elif __GLASGOW_HASKELL__ > 800
          hstype (GHC.L _ (GHC.HsTyVar _ n)) = return (FN [rdrName2NamePure nm n],DN [])
#elif __GLASGOW_HASKELL__ > 710
          hstype (GHC.L _ (GHC.HsTyVar n)) = return (FN [rdrName2NamePure nm n],DN [])
#else
          hstype (GHC.L l (GHC.HsTyVar n)) = return (FN [rdrName2NamePure nm (GHC.L l n)],DN [])
#endif
#if __GLASGOW_HASKELL__ >= 806
          hstype (GHC.L _ (GHC.HsAppTy _ t1 t2)) = recurseList [t1,t2]
          hstype (GHC.L _ (GHC.HsFunTy _ t1 t2)) = recurseList [t1,t2]
          hstype (GHC.L _ (GHC.HsListTy _ typ))  = hsFreeAndDeclaredRdr' nm typ
#else
          hstype (GHC.L _ (GHC.HsAppTy t1 t2)) = recurseList [t1,t2]
          hstype (GHC.L _ (GHC.HsFunTy t1 t2)) = recurseList [t1,t2]
          hstype (GHC.L _ (GHC.HsListTy typ))  = hsFreeAndDeclaredRdr' nm typ
#endif
#if __GLASGOW_HASKELL__ < 806
          hstype (GHC.L _ (GHC.HsPArrTy typ)) = hsFreeAndDeclaredRdr' nm typ
#endif
#if __GLASGOW_HASKELL__ >= 806
          hstype (GHC.L _ (GHC.HsTupleTy _ _ typs)) = recurseList typs
          hstype (GHC.L _ (GHC.HsOpTy _ t1 _ t2)) = recurseList [t1,t2]
          hstype (GHC.L _ (GHC.HsParTy _ typ)) = hsFreeAndDeclaredRdr' nm typ
          hstype (GHC.L _ (GHC.HsIParamTy _ _ typ)) = hsFreeAndDeclaredRdr' nm typ
          hstype (GHC.L _ (GHC.HsStarTy _ _)) = mzero
#else
          hstype (GHC.L _ (GHC.HsTupleTy _ typs)) = recurseList typs
          hstype (GHC.L _ (GHC.HsOpTy t1 _ t2)) = recurseList [t1,t2]
          hstype (GHC.L _ (GHC.HsParTy typ)) = hsFreeAndDeclaredRdr' nm typ
          hstype (GHC.L _ (GHC.HsIParamTy _ typ)) = hsFreeAndDeclaredRdr' nm typ
#endif
#if __GLASGOW_HASKELL__ < 806
          hstype (GHC.L _ (GHC.HsEqTy t1 t2)) = recurseList [t1,t2]
#endif
#if __GLASGOW_HASKELL__ >= 806
          hstype (GHC.L _ (GHC.HsKindSig _ t1 t2)) = recurseList [t1,t2]
#else
          hstype (GHC.L _ (GHC.HsKindSig t1 t2)) = recurseList [t1,t2]
#endif
#if __GLASGOW_HASKELL__ <= 710
          hstype (GHC.L _ (GHC.HsQuasiQuoteTy _)) = return emptyFD
#endif
          hstype (GHC.L _ (GHC.HsSpliceTy _ _)) = return (FN [],DN [])
#if __GLASGOW_HASKELL__ >= 806
          hstype (GHC.L _ (GHC.HsDocTy _ _ typ)) = hsFreeAndDeclaredRdr' nm typ
          hstype (GHC.L _ (GHC.HsBangTy _ _ typ)) = hsFreeAndDeclaredRdr' nm typ
          hstype (GHC.L _ (GHC.HsRecTy _ cons)) = recurseList cons
#else
          hstype (GHC.L _ (GHC.HsDocTy _ typ)) = hsFreeAndDeclaredRdr' nm typ
          hstype (GHC.L _ (GHC.HsBangTy _ typ)) = hsFreeAndDeclaredRdr' nm typ
          hstype (GHC.L _ (GHC.HsRecTy cons)) = recurseList cons
#endif
#if __GLASGOW_HASKELL__ < 806
          hstype (GHC.L _ (GHC.HsCoreTy _)) = return emptyFD
#endif
#if __GLASGOW_HASKELL__ <= 800
          hstype (GHC.L _ (GHC.HsExplicitListTy _ typs)) = recurseList typs
#else
          hstype (GHC.L _ (GHC.HsExplicitListTy _ _ typs)) = recurseList typs
#endif
          hstype (GHC.L _ (GHC.HsExplicitTupleTy _ typs)) = recurseList typs
          hstype (GHC.L _ (GHC.HsTyLit {})) = return emptyFD
#if __GLASGOW_HASKELL__ <= 710
          hstype (GHC.L _ (GHC.HsWrapTy _ typ)) = hsFreeAndDeclaredRdr' nm typ
#endif
#if __GLASGOW_HASKELL__ <= 710
          hstype (GHC.L _ (GHC.HsWildcardTy)) = return (FN [],DN [])
          hstype (GHC.L l (GHC.HsNamedWildcardTy n)) = return (FN [rdrName2NamePure nm (GHC.L l n)],DN [])
#else
          hstype (GHC.L _ (GHC.HsWildCardTy _)) = return (FN [],DN [])
#endif
#if __GLASGOW_HASKELL__ >= 806
          hstype (GHC.L _ (GHC.HsQualTy _ (GHC.L _ ctxt) ty)) = recurseList (ty:ctxt)
#elif __GLASGOW_HASKELL__ > 710
          hstype (GHC.L _ (GHC.HsQualTy (GHC.L _ ctxt) ty)) = recurseList (ty:ctxt)
#endif
#if __GLASGOW_HASKELL__ >= 806
#elif __GLASGOW_HASKELL__ > 710
          hstype (GHC.L _ (GHC.HsAppsTy as)) = do
            fds <- mapM doApp as
            return $ mconcat fds
            where
              doApp (GHC.L _ (GHC.HsAppInfix n)) = return (FN [rdrName2NamePure nm n],DN [])
              doApp (GHC.L _ (GHC.HsAppPrefix ty)) = hstype ty
#endif
#if __GLASGOW_HASKELL__ >= 806
          hstype (GHC.L _ (GHC.HsSumTy _ typs)) = recurseList typs
#elif __GLASGOW_HASKELL__ > 800
          hstype (GHC.L _ (GHC.HsSumTy typs)) = recurseList typs
#endif
#if __GLASGOW_HASKELL__ >= 806
          hstype (GHC.L _ (GHC.XHsType _)) = mzero
#endif

          -- ---------------------------------

          recurseList xs = do
            fds <- mapM (hsFreeAndDeclaredRdr' nm) xs
            return $ mconcat fds

          maybeHelper mv = case mv of
            Nothing -> return (FN [], DN [])
            Just v -> hsFreeAndDeclaredRdr' nm v

          ------------------------------

          addFree :: GHC.Name -> (FreeNames,DeclaredNames)
                  -> Either String (FreeNames,DeclaredNames)
          addFree free (FN fr,de) = return (FN $ [free] `union` fr, de)


-- ---------------------------------------------------------------------

-- |Get the names of all types declared in the given declaration
-- getDeclaredTypesRdr :: GHC.LTyClDecl GHC.RdrName -> RefactGhc [GHC.Name]
getDeclaredTypesRdr :: GHC.LHsDecl GhcPs -> RefactGhc [GHC.Name]
#if __GLASGOW_HASKELL__ >= 806
getDeclaredTypesRdr (GHC.L _ (GHC.TyClD _ decl)) = do
#else
getDeclaredTypesRdr (GHC.L _ (GHC.TyClD decl)) = do
#endif
  nm <- getRefactNameMap
  case decl of
#if __GLASGOW_HASKELL__ >= 806
    (GHC.FamDecl _ (GHC.FamilyDecl _ _ ln _ _ _ _)) -> return [rdrName2NamePure nm ln]
    (GHC.FamDecl _ (GHC.XFamilyDecl _)) -> mzero
    (GHC.SynDecl _ ln  _ _ _) -> return [rdrName2NamePure nm ln]
    (GHC.XTyClDecl _) -> mzero
#elif __GLASGOW_HASKELL__ > 800
    (GHC.FamDecl (GHC.FamilyDecl _ ln _ _ _ _)) -> return [rdrName2NamePure nm ln]
    (GHC.SynDecl ln  _ _ _ _) -> return [rdrName2NamePure nm ln]
#elif __GLASGOW_HASKELL__ > 710
    (GHC.FamDecl (GHC.FamilyDecl _ ln _ _ _)) -> return [rdrName2NamePure nm ln]
    (GHC.SynDecl ln  _ _ _) -> return [rdrName2NamePure nm ln]
#else
    (GHC.FamDecl (GHC.FamilyDecl _ ln _ _)) -> return [rdrName2NamePure nm ln]
    (GHC.SynDecl ln  _ _ _ ) -> return [rdrName2NamePure nm ln]
#endif

#if __GLASGOW_HASKELL__ >= 806
    (GHC.DataDecl _ ln _ _ defn) -> do
      let dds = concatMap (GHC.getConNames . GHC.unLoc) $ GHC.dd_cons defn
#elif __GLASGOW_HASKELL__ > 800
    (GHC.DataDecl ln _ _ defn _ _) -> do
      let dds = concatMap (GHC.getConNames . GHC.unLoc) $ GHC.dd_cons defn
#elif __GLASGOW_HASKELL__ > 710
    (GHC.DataDecl ln _ defn _ _) -> do
      let dds = concatMap (GHC.getConNames . GHC.unLoc) $ GHC.dd_cons defn
#else
    (GHC.DataDecl ln _ defn _) -> do
      let dds = concatMap (GHC.con_names . GHC.unLoc) $ GHC.dd_cons defn
#endif
      let ddns = map (rdrName2NamePure nm) dds
      return $ [rdrName2NamePure nm ln] ++ ddns

#if __GLASGOW_HASKELL__ >= 806
    (GHC.ClassDecl _ _ ln _vars _fixity _fds sigs meths ats _atdefs _) -> do
#elif __GLASGOW_HASKELL__ > 800
    (GHC.ClassDecl _ ln _vars _fixity _fds sigs meths ats _atdefs _ _fvs) -> do
#else
    (GHC.ClassDecl _ ln _vars _fds sigs meths ats _atdefs _ _fvs) -> do
#endif
      -- msn <- getMsn meths
      let msn = getDeclaredVarsRdr nm (map wrapDecl $ GHC.bagToList meths)
      let fds = map (GHC.fdLName . GHC.unLoc) ats
          fds' = map (rdrName2NamePure nm) fds
      return $ nub $ [rdrName2NamePure nm ln] ++ ssn ++ msn ++ fds' -- ++ asn
      where
        getLSig :: GHC.LSig GhcPs -> [GHC.Name]
#if __GLASGOW_HASKELL__ >= 806
        getLSig (GHC.L _ (GHC.TypeSig _ ns _))  = map (rdrName2NamePure nm) ns
#elif __GLASGOW_HASKELL__ > 710
        getLSig (GHC.L _ (GHC.TypeSig ns _))  = map (rdrName2NamePure nm) ns
#else
        getLSig (GHC.L _ (GHC.TypeSig ns _ _))  = map (rdrName2NamePure nm) ns
#endif
#if __GLASGOW_HASKELL__ >= 806
        getLSig (GHC.L _ (GHC.ClassOpSig _ _ ns _)) = map (rdrName2NamePure nm) ns
#elif __GLASGOW_HASKELL__ > 710
        getLSig (GHC.L _ (GHC.ClassOpSig _ ns _)) = map (rdrName2NamePure nm) ns
#else
        getLSig (GHC.L _ (GHC.GenericSig ns _)) = map (rdrName2NamePure nm) ns
#endif
        getLSig (GHC.L _ (GHC.IdSig {})) = []
#if __GLASGOW_HASKELL__ >= 806
        getLSig (GHC.L _ (GHC.InlineSig _ ln2 _)) = [rdrName2NamePure nm ln2]
        getLSig (GHC.L _ (GHC.SpecSig _ ln2 _ _)) = [rdrName2NamePure nm ln2]
        getLSig (GHC.L _ (GHC.SpecInstSig _ _ _)) = []
#else
        getLSig (GHC.L _ (GHC.InlineSig ln2 _)) = [rdrName2NamePure nm ln2]
        getLSig (GHC.L _ (GHC.SpecSig ln2 _ _)) = [rdrName2NamePure nm ln2]
        getLSig (GHC.L _ (GHC.SpecInstSig _ _)) = []
#endif
        getLSig (GHC.L _ (GHC.FixSig {})) = []
#if __GLASGOW_HASKELL__ >= 806
        getLSig (GHC.L _ (GHC.PatSynSig _ _ _))  = error "To implement: getLSig PatSynSig"
#elif __GLASGOW_HASKELL__ > 710
        getLSig (GHC.L _ (GHC.PatSynSig _ _))  = error "To implement: getLSig PatSynSig"
#else
        getLSig (GHC.L _ (GHC.PatSynSig _ _ _ _ _)) = error "To implement: getLSig PatSynSig"
#endif
        getLSig (GHC.L _ (GHC.MinimalSig {})) = error "To implement: getLSig PatSynSig"
#if __GLASGOW_HASKELL__ > 800
        getLSig (GHC.L _ (GHC.SCCFunSig {})) = []
        getLSig (GHC.L _ (GHC.CompleteMatchSig {})) = []
#endif
#if __GLASGOW_HASKELL__ >= 806
        getLSig (GHC.L _ (GHC.XSig _))  = []
#endif

        ssn = concatMap getLSig sigs
getDeclaredTypesRdr _ = return []

-- ---------------------------------------------------------------------

-- | Return True if the specified Name ocuurs in the given syntax phrase.
findNameInRdr :: (SYB.Data t) => NameMap -> GHC.Name -> t -> Bool
findNameInRdr nm pn t = findNamesRdr nm [pn] t

-- ---------------------------------------------------------------------

-- | Return True if any of the specified PNames ocuur in the given syntax phrase.
findNamesRdr :: (SYB.Data t) => NameMap -> [GHC.Name] -> t -> Bool
findNamesRdr nm pns t =
  isJust $ SYB.something (inName) t
    where
      -- r = (SYB.everythingStaged SYB.Parser mappend mempty (inName) t)

      checker :: GHC.Located GHC.RdrName -> Maybe Bool
      checker ln
         | elem (GHC.nameUnique (rdrName2NamePure nm ln)) uns = Just True
      checker _ = Nothing

      inName :: (SYB.Typeable a) => a -> Maybe Bool
      inName = nameSybQuery checker

      uns = map GHC.nameUnique pns

-- ---------------------------------------------------------------------

definedPNsRdr :: GHC.LHsDecl GhcPs -> [GHC.Located GHC.RdrName]
#if __GLASGOW_HASKELL__ >= 806
definedPNsRdr (GHC.L _ (GHC.ValD _ (GHC.FunBind _ pname _ _ _)))  = [pname]
#elif __GLASGOW_HASKELL__ > 710
definedPNsRdr (GHC.L _ (GHC.ValD (GHC.FunBind pname _ _ _ _)))   = [pname]
#else
definedPNsRdr (GHC.L _ (GHC.ValD (GHC.FunBind pname _ _ _ _ _)))   = [pname]
#endif
#if __GLASGOW_HASKELL__ >= 806
definedPNsRdr (GHC.L _ (GHC.ValD _ (GHC.PatBind _ p _rhs _ty))) = (hsNamessRdr p)
#else
definedPNsRdr (GHC.L _ (GHC.ValD (GHC.PatBind p _rhs _ty _fvs _))) = (hsNamessRdr p)
#endif
-- VarBind will never occur in ParsedSource
-- TODO: what about GHC.AbsBinds?
definedPNsRdr  _ = []

-- ---------------------------------------------------------------------

definedNamesRdr :: NameMap -> GHC.LHsDecl GhcPs -> [GHC.Name]
definedNamesRdr nameMap bind = map (rdrName2NamePure nameMap) (definedPNsRdr bind)

-- ---------------------------------------------------------------------

-- |Find those declarations(function\/pattern binding) which define
-- the specified GHC.Names. incTypeSig indicates whether the
-- corresponding type signature will be included.
definingDeclsRdrNames ::
            NameMap
            -> [GHC.Name]   -- ^ The specified identifiers.
            -> [GHC.LHsDecl GhcPs] -- ^ A collection of declarations.
            -> Bool       -- ^ True means to include the type signature.
            -> Bool       -- ^ True means to look at the local declarations as well.
            -> [GHC.LHsDecl GhcPs]  -- ^ The result.
definingDeclsRdrNames nameMap pns ds incTypeSig recursive = concatMap defining ds
-- TODO: Maybe Use hsFreeAndDeclaredRdr to see what is declared in a decl. Recursive?
  where
   defining decl
     = if recursive
        then SYB.everything  (++) ([]  `SYB.mkQ` definesDecl `SYB.extQ` definesBind)  decl
        else definesDecl decl
     where
      definesDecl :: (GHC.LHsDecl GhcPs) -> [GHC.LHsDecl GhcPs]
#if __GLASGOW_HASKELL__ >= 806
      definesDecl decl'@(GHC.L _ (GHC.ValD _ (GHC.FunBind {})))
#elif __GLASGOW_HASKELL__ > 710
      definesDecl decl'@(GHC.L _ (GHC.ValD (GHC.FunBind _ _ _ _ _)))
#else
      definesDecl decl'@(GHC.L _ (GHC.ValD (GHC.FunBind _ _ _ _ _ _)))
#endif
        | any (\n -> definesDeclRdr nameMap n decl') pns = [decl']

#if __GLASGOW_HASKELL__ >= 806
      definesDecl decl'@(GHC.L _l (GHC.ValD _ (GHC.PatBind {})))
#else
      definesDecl decl'@(GHC.L _l (GHC.ValD (GHC.PatBind _p _rhs _ty _fvs _)))
#endif
        | any (\n -> definesDeclRdr nameMap n decl') pns = [decl']

      definesDecl decl'@(GHC.L _l (GHC.TyClD {}))
        | any (\n -> definesNameRdr nameMap n decl') pns = [decl']

      definesDecl decl'@(GHC.L _l (GHC.SigD {}))
        | incTypeSig && any (\n -> definesNameRdr nameMap n decl') pns = [decl']

      definesDecl _ = []

      definesBind :: (GHC.LHsBind GhcPs) -> [GHC.LHsDecl GhcPs]
#if __GLASGOW_HASKELL__ >= 806
      definesBind (GHC.L l b) = definesDecl (GHC.L l (GHC.ValD GHC.noExt b))
#else
      definesBind (GHC.L l b) = definesDecl (GHC.L l (GHC.ValD b))
#endif

-- ---------------------------------------------------------------------

-- |Find those declarations(function\/pattern binding) which define
-- the specified GHC.Names. incTypeSig indicates whether the corresponding type
-- signature will be included.
definingDeclsRdrNames' :: (SYB.Data t)
            => NameMap
            -> [GHC.Name]   -- ^ The specified identifiers.
            -> t            -- ^ A collection of declarations.
            -> [GHC.LHsDecl GhcPs]  -- ^ The result.
definingDeclsRdrNames' nameMap pns ds = defining ds
  where
   defining decl
     = SYB.everything (++) ([]  `SYB.mkQ` defines' `SYB.extQ` definesBind) decl
     where
      defines' :: (GHC.LHsDecl GhcPs) -> [GHC.LHsDecl GhcPs]
#if __GLASGOW_HASKELL__ >= 806
      defines' decl'@(GHC.L _ (GHC.ValD _ (GHC.FunBind{})))
#else
      defines' decl'@(GHC.L _ (GHC.ValD (GHC.FunBind{})))
#endif
        | any (\n -> definesDeclRdr nameMap n decl') pns = [decl']

#if __GLASGOW_HASKELL__ >= 806
      defines' decl'@(GHC.L _l (GHC.ValD _ (GHC.PatBind {})))
#else
      defines' decl'@(GHC.L _l (GHC.ValD (GHC.PatBind {})))
#endif
        | any (\n -> definesDeclRdr nameMap n decl') pns = [decl']

      defines' decl'@(GHC.L _l (GHC.TyClD {}))
        | any (\n -> definesDeclRdr nameMap n decl') pns = [decl']

      defines' _ = []

      definesBind :: (GHC.LHsBind GhcPs) -> [GHC.LHsDecl GhcPs]
#if __GLASGOW_HASKELL__ >= 806
      definesBind (GHC.L l b) = defines' (GHC.L l (GHC.ValD GHC.noExt b))
#else
      definesBind (GHC.L l b) = defines' (GHC.L l (GHC.ValD b))
#endif

-- ---------------------------------------------------------------------

-- |Find those type signatures for the specified GHC.Names.
definingSigsRdrNames :: (SYB.Data t) =>
            NameMap
            ->[GHC.Name] -- ^ The specified identifiers.
            ->t        -- ^ A collection of declarations.
            ->[GHC.LSig GhcPs]  -- ^ The result.
definingSigsRdrNames nameMap pns ds = def ds
  where
   def decl
     -- = SYB.everythingStaged SYB.Renamer (++) [] ([]  `SYB.mkQ` inSig) decl
     = SYB.everything (++) ([]  `SYB.mkQ` inSig `SYB.extQ` inSigDecl) decl
     where
      inSigDecl :: GHC.LHsDecl GhcPs -> [GHC.LSig GhcPs]
#if __GLASGOW_HASKELL__ >= 806
      inSigDecl (GHC.L l (GHC.SigD _ s)) = inSig (GHC.L l s)
#else
      inSigDecl (GHC.L l (GHC.SigD s)) = inSig (GHC.L l s)
#endif
      inSigDecl _ = []

      inSig :: (GHC.LSig GhcPs) -> [GHC.LSig GhcPs]
#if __GLASGOW_HASKELL__ >= 806
      inSig (GHC.L l (GHC.TypeSig x ns t))
       | defines' ns /= [] = [(GHC.L l (GHC.TypeSig x (defines' ns) t))]
#elif __GLASGOW_HASKELL__ > 710
      inSig (GHC.L l (GHC.TypeSig ns t))
       | defines' ns /= [] = [(GHC.L l (GHC.TypeSig (defines' ns) t))]
#else
      inSig (GHC.L l (GHC.TypeSig ns t p))
       | defines' ns /= [] = [(GHC.L l (GHC.TypeSig (defines' ns) t p))]
#endif
      inSig _ = []

      defines' :: [GHC.Located GHC.RdrName] -> [GHC.Located GHC.RdrName]
      defines' p
        -- = filter (\(GHC.L _ n) -> n `elem` pns) p
        = let
             isDefined :: GHC.Located GHC.RdrName -> [GHC.Located GHC.RdrName]
             isDefined ln = if (rdrName2NamePure nameMap ln) `elem` pns
                              then [ln]
                              else []
          in concatMap isDefined p

-- ---------------------------------------------------------------------

-- |Find those declarations which define the specified GHC.Names.
definingTyClDeclsNames:: (SYB.Data t)
            => NameMap
            -> [GHC.Name]   -- ^ The specified identifiers.
            -> t -- ^ A collection of declarations.
            -> [GHC.LTyClDecl GhcPs]  -- ^ The result.
definingTyClDeclsNames nm pns t = defining t
  where
   defining decl
     = SYB.everything (++)
                   ([]  `SYB.mkQ` defines'
                        `SYB.extQ` definesDecl) decl
     where
      defines' :: (GHC.LTyClDecl GhcPs) -> [GHC.LTyClDecl GhcPs]
#if __GLASGOW_HASKELL__ >= 806
      defines' decl'@(GHC.L _ (GHC.FamDecl _ (GHC.FamilyDecl _ _ pname _ _ _ _)))
#elif __GLASGOW_HASKELL__ > 800
      defines' decl'@(GHC.L _ (GHC.FamDecl (GHC.FamilyDecl _ pname _ _ _ _)))
#elif __GLASGOW_HASKELL__ > 710
      defines' decl'@(GHC.L _ (GHC.FamDecl (GHC.FamilyDecl _ pname _ _ _)))
#else
      defines' decl'@(GHC.L _ (GHC.FamDecl (GHC.FamilyDecl _ pname _ _)))
#endif
        | elem (GHC.nameUnique $ rdrName2NamePure nm pname) uns = [decl']
        | otherwise = []
#if __GLASGOW_HASKELL__ >= 806
      defines' (GHC.L _ (GHC.FamDecl _ (GHC.XFamilyDecl {}))) = []
      defines' (GHC.L _ (GHC.XTyClDecl _)) = []
#endif

#if __GLASGOW_HASKELL__ >= 806
      defines' decl'@(GHC.L _ (GHC.SynDecl _ pname _ _ _))
#elif __GLASGOW_HASKELL__ > 800
      defines' decl'@(GHC.L _ (GHC.SynDecl pname _ _ _ _))
#else
      defines' decl'@(GHC.L _ (GHC.SynDecl pname _ _ _))
#endif
        | elem (GHC.nameUnique $ rdrName2NamePure nm pname) uns = [decl']
        | otherwise = []

      defines' decl'@(GHC.L _ (GHC.DataDecl {}))
        --   elem (GHC.nameUnique $ rdrName2NamePure nm pname) uns = [decl']
        | not $ null (dus `intersect` uns) = [decl']
        | otherwise = []
        where
          (_,DN ds) = hsFreeAndDeclaredRdr nm decl'
          dus = map GHC.nameUnique ds

#if __GLASGOW_HASKELL__ >= 806
      defines' decl'@(GHC.L _ (GHC.ClassDecl _ _ pname _ _ _ _ _ _ _ _))
#elif __GLASGOW_HASKELL__ > 800
      defines' decl'@(GHC.L _ (GHC.ClassDecl _ pname _ _ _ _ _ _ _ _ _))
#else
      defines' decl'@(GHC.L _ (GHC.ClassDecl _ pname _ _ _ _ _ _ _ _))
#endif
        | elem (GHC.nameUnique $ rdrName2NamePure nm pname) uns = [decl']
        | otherwise = []

#if __GLASGOW_HASKELL__ >= 806
      definesDecl (GHC.L l (GHC.TyClD _ d)) = defines' (GHC.L l d)
#else
      definesDecl (GHC.L l (GHC.TyClD d)) = defines' (GHC.L l d)
#endif
      definesDecl _ = []

      uns = map (\n -> GHC.nameUnique n) pns

-- ---------------------------------------------------------------------

-- | Return True if the function\/pattern binding defines the
-- specified identifier.
definesRdr :: NameMap -> GHC.Name -> GHC.LHsBind GhcPs -> Bool
definesRdr nm nin (GHC.L _ (GHC.FunBind { GHC.fun_id = ln }))
  = GHC.nameUnique (rdrName2NamePure nm ln) == GHC.nameUnique nin
definesRdr nm n (GHC.L _ (GHC.PatBind { GHC.pat_lhs = p } ))
  = elem n (map (rdrName2NamePure nm) (hsNamessRdr p))
definesRdr _ _ _= False


-- |Unwraps a LHsDecl and calls definesRdr on the result if a HsBind or calls clsDeclDefinesRdr if a TyClD
definesDeclRdr :: NameMap -> GHC.Name -> GHC.LHsDecl GhcPs -> Bool
#if __GLASGOW_HASKELL__ >= 806
definesDeclRdr nameMap nin (GHC.L l (GHC.ValD _ d)) = definesRdr nameMap nin (GHC.L l d)
definesDeclRdr nameMap nin (GHC.L _ (GHC.TyClD _ ty)) = clsDeclDefinesRdr nameMap nin ty
#else
definesDeclRdr nameMap nin (GHC.L l (GHC.ValD d)) = definesRdr nameMap nin (GHC.L l d)
definesDeclRdr nameMap nin (GHC.L _ (GHC.TyClD ty)) = clsDeclDefinesRdr nameMap nin ty
#endif
definesDeclRdr _ _ _ = False

-- | Return True of the type class declaration defines the
-- specified identifier
clsDeclDefinesRdr :: NameMap -> GHC.Name -> GHC.TyClDecl GhcPs -> Bool
clsDeclDefinesRdr nameMap nin (GHC.SynDecl { GHC.tcdLName = (GHC.L ln _nm) }) =
  case Map.lookup ln nameMap of
    Nothing -> False
    Just n  -> GHC.nameUnique n == GHC.nameUnique nin
clsDeclDefinesRdr _ _ _ = False

-- | Returns True if the provided Name is defined in the LHsDecl
definesNameRdr :: NameMap -> GHC.Name -> GHC.LHsDecl GhcPs -> Bool
#if __GLASGOW_HASKELL__ >= 806
definesNameRdr nameMap nin (GHC.L l (GHC.ValD _ d))  = definesRdr nameMap nin (GHC.L l d)
#else
definesNameRdr nameMap nin (GHC.L l (GHC.ValD d))  = definesRdr nameMap nin (GHC.L l d)
#endif
definesNameRdr nameMap nin d = nin `elem` declared
  where
    (_,DN declared) = hsFreeAndDeclaredRdr nameMap d
-- definesDeclRdr _ _ _ = False

-- ---------------------------------------------------------------------

-- | Return True if the declaration defines the type signature of the
-- specified identifier.
definesTypeSigRdr :: NameMap -> GHC.Name -> GHC.Sig GhcPs -> Bool
#if __GLASGOW_HASKELL__ >= 806
definesTypeSigRdr nameMap pn (GHC.TypeSig _ names _typ)
#elif __GLASGOW_HASKELL__ > 710
definesTypeSigRdr nameMap pn (GHC.TypeSig names _typ)
#else
definesTypeSigRdr nameMap pn (GHC.TypeSig names _typ _)
#endif
  = elem (GHC.nameUnique pn) (map (GHC.nameUnique . rdrName2NamePure nameMap) names)
-- definesTypeSigRdr _ _  _ = False
definesTypeSigRdr _ _  x = error $ "definesTypeSigRdr : got " ++ showAnnData mempty 0 x

-- |Unwraps a LHsDecl and calls definesRdr on the result if a Sig
definesSigDRdr :: NameMap -> GHC.Name -> GHC.LHsDecl GhcPs -> Bool
#if __GLASGOW_HASKELL__ >= 806
definesSigDRdr nameMap nin (GHC.L _ (GHC.SigD _ d)) = definesTypeSigRdr nameMap nin d
#else
definesSigDRdr nameMap nin (GHC.L _ (GHC.SigD d)) = definesTypeSigRdr nameMap nin d
#endif
definesSigDRdr _ _ _ = False

-- ---------------------------------------------------------------------

-- |Get all the names in the given syntax element
hsNamessRdr :: (SYB.Data t) => t -> [GHC.Located GHC.RdrName]
hsNamessRdr t = nub $ fromMaybe [] r
  where
     r = (SYB.everything mappend (inName) t)

     checker :: GHC.Located GHC.RdrName -> Maybe [GHC.Located GHC.RdrName]
     checker x = Just [x]

     inName :: (SYB.Typeable a) => a -> Maybe [GHC.Located GHC.RdrName]
     inName = nameSybQuery checker

-- ---------------------------------------------------------------------

-- |Does the given 'GHC.Name' appear as a 'GHC.Located' 'GHC.RdrName' anywhere in 't'?
findLRdrName :: (SYB.Data t) => NameMap -> GHC.Name -> t -> Bool
findLRdrName nm n t = isJust $ SYB.something isMatch t
  where
    checker :: GHC.Located GHC.RdrName -> Maybe Bool
    checker x
      | GHC.nameUnique (rdrName2NamePure nm x) == GHC.nameUnique n = Just True
      | otherwise = Nothing

    isMatch :: (SYB.Typeable a) => a -> Maybe Bool
    isMatch = nameSybQuery checker

-- ---------------------------------------------------------------------


getDeclaredVarsRdr :: NameMap -> [GHC.LHsDecl GhcPs] -> [GHC.Name]
getDeclaredVarsRdr nm bs = concatMap vars bs
  where
      vars :: (GHC.LHsDecl GhcPs) -> [GHC.Name]
#if __GLASGOW_HASKELL__ >= 806
      vars (GHC.L _ (GHC.ValD _ (GHC.FunBind _ ln _ _ _ )))   = [rdrName2NamePure nm ln]
      vars (GHC.L _ (GHC.ValD _ (GHC.PatBind _ p _rhs _ ))) = (map (rdrName2NamePure nm) $ hsNamessRdr p)
#elif __GLASGOW_HASKELL__ > 710
      vars (GHC.L _ (GHC.ValD (GHC.FunBind ln _ _ _ _fvs)))   = [rdrName2NamePure nm ln]
      vars (GHC.L _ (GHC.ValD (GHC.PatBind p _rhs _ty _fvs _))) = (map (rdrName2NamePure nm) $ hsNamessRdr p)
#else
      vars (GHC.L _ (GHC.ValD (GHC.FunBind ln _ _ _ _fvs _)))   = [rdrName2NamePure nm ln]
      vars (GHC.L _ (GHC.ValD (GHC.PatBind p _rhs _ty _fvs _))) = (map (rdrName2NamePure nm) $ hsNamessRdr p)
#endif
      vars _ = []

--------------------------------------------------------------------------------
-- | Same as `hsVisiblePNsRdr' except that the returned identifiers are
-- in String format.
hsVisibleNamesRdr:: (SYB.Data t2)
  => GHC.Name -> t2 -> RefactGhc [String]
hsVisibleNamesRdr e t = do
    nm <- getRefactNameMap
    (DN d) <- hsVisibleDsRdr nm e t
    return ((nub . map showGhc) d)

------------------------------------------------------------------------

-- | Given a 'Name' n and a syntax phrase t, if n occurs in t, then return those
-- variables which are declared in t and accessible to n, otherwise
-- return [].
hsVisibleDsRdr :: (SYB.Data t)
             => NameMap -> GHC.Name -> t -> RefactGhc DeclaredNames
-- TODO: DeclaredNames is probably the wrong type. Perhaps create VisibleNames
--       And perhaps use a NameSet or Set, to avoid the nub call
hsVisibleDsRdr nm e t = do
  -- logDataWithAnns "hsVisibleDsRdr:(e,t)=" (e,t)
  (DN d) <- res
  return (DN (nub d))
  where
    -- TODO: this is effectively a recursive descent approach, where
    --       each syntax element processor knows exactly what it needs
    --       in terms of sub-elements. Hence as an optimisation,
    --       consider calling the relevent element directly, instead
    --       of looping back into the main function.
    --  OR, use a more effective recursion scheme
    res = (const err -- (DN [])
          `SYB.extQ` parsed
          `SYB.extQ` lvalbinds
          `SYB.extQ` valbinds
          `SYB.extQ` lhsdecls
          `SYB.extQ` lhsdecl
          `SYB.extQ` lhsbindslr
          `SYB.extQ` hsbinds
          `SYB.extQ` hsbind
          `SYB.extQ` hslocalbinds
          `SYB.extQ` lmatch
          `SYB.extQ` match
          `SYB.extQ` grhss
          `SYB.extQ` lgrhs
          `SYB.extQ` lexpr
          `SYB.extQ` tyclgroups
          `SYB.extQ` tyclgroup
          `SYB.extQ` tycldeclss
          `SYB.extQ` tycldecls
          `SYB.extQ` ltycldecl
          `SYB.extQ` tycldecl
          `SYB.extQ` hsdatadefn
          `SYB.extQ` condecl
          `SYB.extQ` instdecls
          `SYB.extQ` instdecl
          `SYB.extQ` lhstype
          `SYB.extQ` lsigs
          `SYB.extQ` lsig
          `SYB.extQ` lstmts
          `SYB.extQ` lstmt
          `SYB.extQ` lpats
          `SYB.extQ` lpat
#if __GLASGOW_HASKELL__ > 710
          `SYB.extQ` ibndrs
          `SYB.extQ` lsigty
#endif
          `SYB.extQ` lanndecl
          ) t

    -- err2 = error $ "hsVisibleDsRdr:err2:no match for:" ++ (SYB.showData SYB.Renamer 0 t)

    parsed :: GHC.ParsedSource -> RefactGhc DeclaredNames
    parsed p
      | findNameInRdr nm e p = do
         -- dfds <- mapM (hsVisibleDsRdr nm e) $ GHC.hsmodDecls $ GHC.unLoc p
         -- logm $ "hsVisibleDsRdr parsedSource:decls starting"
         dfds <- mapM (declFun ( hsVisibleDsRdr nm e) ) $ GHC.hsmodDecls $ GHC.unLoc p
         -- logm $ "hsVisibleDsRdr parsedSource:decls done"
         return $ mconcat dfds
    parsed _ = return (DN [])

    lvalbinds :: (GHC.Located (GHC.HsLocalBinds GhcPs)) -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    lvalbinds (GHC.L _ (GHC.HsValBinds _ vb)) = valbinds vb
    lvalbinds (GHC.L _ (GHC.HsIPBinds {}))   = return (DN [])
    lvalbinds (GHC.L _ GHC.EmptyLocalBinds{}) = return (DN [])
    lvalbinds (GHC.L _ GHC.XHsLocalBindsLR{}) = return (DN [])
#else
    lvalbinds (GHC.L _ (GHC.HsValBinds vb)) = valbinds vb
    lvalbinds (GHC.L _ (GHC.HsIPBinds _))   = return (DN [])
    lvalbinds (GHC.L _ GHC.EmptyLocalBinds) = return (DN [])
#endif

    valbinds :: (GHC.HsValBinds GhcPs) -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    valbinds vb@(GHC.ValBinds _ bindsBag sigs)
      | findNameInRdr nm e vb = do
          fdsb <- mapM (hsVisibleDsRdr nm e) $ GHC.bagToList bindsBag
          fdss <- mapM (hsVisibleDsRdr nm e) sigs
          return $ mconcat fdss <> mconcat fdsb
#else
    valbinds vb@(GHC.ValBindsIn bindsBag sigs)
      | findNameInRdr nm e vb = do
          fdsb <- mapM (hsVisibleDsRdr nm e) $ GHC.bagToList bindsBag
          fdss <- mapM (hsVisibleDsRdr nm e) sigs
          return $ mconcat fdss <> mconcat fdsb
    valbinds vb@(GHC.ValBindsOut _binds _sigs)
      | findNameInRdr nm e vb = do
          -- logm $ "hsVisibleDsRdr valbinds:ValBindsOut:impossible for RdrName"
          return (DN [])
#endif

    valbinds _ = do
      logm $ "hsVisibleDsRdr nm.valbinds:not matched"
      return (DN [])

    lhsdecls :: [GHC.LHsDecl GhcPs] -> RefactGhc DeclaredNames
    lhsdecls ds
      | findNameInRdr nm e ds = do
         dfds <- mapM (declFun ( hsVisibleDsRdr nm e) ) ds
         return $ mconcat dfds
    lhsdecls _ = return (DN [])

    lhsdecl :: GHC.LHsDecl GhcPs -> RefactGhc DeclaredNames
    lhsdecl (GHC.L l dd) = do
        -- logm $ "hsVisibleDsRdr.lhsdecl"
        case dd of
#if __GLASGOW_HASKELL__ >= 806
            GHC.TyClD _ d       -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.InstD _ d       -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.DerivD _ d      -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.ValD _ d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.SigD _ d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.DefD _ d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.ForD _ d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.WarningD _ d    -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.AnnD _ d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.RuleD _ d       -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.SpliceD _ d     -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.DocD _ d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.RoleAnnotD _ d  -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.XHsDecl _       -> return mempty
#else
            GHC.TyClD d       -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.InstD d       -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.DerivD d      -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.ValD d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.SigD d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.DefD d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.ForD d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.WarningD d    -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.AnnD d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.RuleD d       -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.VectD d       -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.SpliceD d     -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.DocD d        -> hsVisibleDsRdr nm e (GHC.L l d)
            GHC.RoleAnnotD d  -> hsVisibleDsRdr nm e (GHC.L l d)
#endif
#if __GLASGOW_HASKELL__ < 711
            GHC.QuasiQuoteD d -> hsVisibleDsRdr nm e (GHC.L l d)
#endif

    lhsbindslr :: GHC.LHsBinds GhcPs -> RefactGhc DeclaredNames
    lhsbindslr bs = do
      fds <- mapM (hsVisibleDsRdr nm e) $ GHC.bagToList bs
      return $ mconcat fds

    hsbinds :: [GHC.LHsBind GhcPs] -> RefactGhc DeclaredNames
    hsbinds ds
      | findNameInRdr nm e ds = do
        fds <- mapM (hsVisibleDsRdr nm e) ds
        return $ mconcat fds
    hsbinds _ = return (DN [])

    hsbind :: (GHC.LHsBind GhcPs) -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    hsbind ((GHC.L _ (GHC.FunBind _ _n (GHC.MG _ (GHC.L _ matches) _) _ _)))
#elif __GLASGOW_HASKELL__ > 710
    hsbind ((GHC.L _ (GHC.FunBind _n (GHC.MG (GHC.L _ matches) _ _ _) _ _ _)))
#else
    hsbind ((GHC.L _ (GHC.FunBind _n _ (GHC.MG matches _ _ _) _ _ _)))
#endif
      | findNameInRdr nm e matches = do
          fds <- mapM (hsVisibleDsRdr nm e) matches
          return $ mconcat fds
    hsbind _ = do
      return (DN [])


    hslocalbinds :: (GHC.HsLocalBinds GhcPs) -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    hslocalbinds (GHC.HsValBinds _ binds)
      | findNameInRdr nm e binds = hsVisibleDsRdr nm e binds
    hslocalbinds (GHC.HsIPBinds _ binds)
      | findNameInRdr nm e binds = hsVisibleDsRdr nm e binds
    hslocalbinds (GHC.EmptyLocalBinds{}) = return (DN [])
#else
    hslocalbinds (GHC.HsValBinds binds)
      | findNameInRdr nm e binds = hsVisibleDsRdr nm e binds
    hslocalbinds (GHC.HsIPBinds binds)
      | findNameInRdr nm e binds = hsVisibleDsRdr nm e binds
    hslocalbinds (GHC.EmptyLocalBinds) = return (DN [])
#endif
    hslocalbinds _ = return (DN [])

    lmatch :: (GHC.LMatch GhcPs (GHC.LHsExpr GhcPs)) -> RefactGhc DeclaredNames
    lmatch (GHC.L _ m) = match m

    match :: (GHC.Match GhcPs (GHC.LHsExpr GhcPs)) -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    match (GHC.Match _ _fn pats rhs)
#elif __GLASGOW_HASKELL__ >= 804
    match (GHC.Match _fn pats rhs)
#else
    match (GHC.Match _fn pats _mtyp rhs)
#endif
      | findNameInRdr nm e rhs || findNameInRdr nm e pats = do
           -- logm $ "hsVisibleDsRdr nm.lmatch:doing rhs"
           let (_pf,pd) = hsFreeAndDeclaredRdr nm pats
           -- logm $ "hsVisibleDsRdr nm.lmatch:(pf,pd)=" ++ (show (_pf,pd))
           rd <- hsVisibleDsRdr nm e rhs
           -- logm $ "hsVisibleDsRdr nm.lmatch:rd=" ++ (show rd)
           return (pd <> rd)
    match _ =return  (DN [])

    grhss :: (GHC.GRHSs GhcPs (GHC.LHsExpr GhcPs)) -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    grhss (GHC.GRHSs _ guardedRhss lstmts')
#else
    grhss (GHC.GRHSs guardedRhss lstmts')
#endif
      | findNameInRdr nm e guardedRhss || findNameInRdr nm e lstmts' = do
          -- logm "hsVisibleDsRdr nm.grhss:about to do lstmts"
          fds <- mapM (hsVisibleDsRdr nm e) guardedRhss
          -- sfds <- hsVisibleDsRdr nm e lstmts'
          let (_,sfds) = hsFreeAndDeclaredRdr nm lstmts'
          return $ mconcat (sfds:fds)
    grhss _ = do
      -- logm $ "hsVisibleDsRdr.grhss: no match"
      return (DN [])

    lgrhs :: GHC.LGRHS GhcPs (GHC.LHsExpr GhcPs) -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    lgrhs (GHC.L _ (GHC.GRHS _ guards ex))
#else
    lgrhs (GHC.L _ (GHC.GRHS guards ex))
#endif
      | findNameInRdr nm e guards = hsVisibleDsRdr nm e guards
      | findNameInRdr nm e ex     = do
        r <- hsVisibleDsRdr nm e ex
        -- logm $ "hsVisibleDsRdr.lgrhs:r=" ++ show r
        return r
    lgrhs _ = do
      -- logm $ "hsVisibleDsRdr.lgrhs: no match"
      return (DN [])


    lexpr :: GHC.LHsExpr GhcPs -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    lexpr (GHC.L l (GHC.HsVar _ (GHC.L _ n)))
#elif __GLASGOW_HASKELL__ > 710
    lexpr (GHC.L l (GHC.HsVar (GHC.L _ n)))
#else
    lexpr (GHC.L l (GHC.HsVar n))
#endif
      | findNameInRdr nm e n  = do
        -- logm $ "hsVisibleDsRdr.lexpr.HsVar entity found"
        return (DN [rdrName2NamePure nm (GHC.L l n)])
#if __GLASGOW_HASKELL__ >= 806
    lexpr (GHC.L _ (GHC.HsLet _ lbinds expr))
#else
    lexpr (GHC.L _ (GHC.HsLet lbinds expr))
#endif
      | findNameInRdr nm e lbinds || findNameInRdr nm e expr  = do
        -- logm $ "hsVisibleDsRdr.lexpr.HsLet entity found"
        let (_,lds) = hsFreeAndDeclaredRdr nm lbinds
        let (_,eds) = hsFreeAndDeclaredRdr nm expr
        return $ lds <> eds

    lexpr expr
      | findNameInRdr nm e expr = do
        -- logm $ "hsVisibleDsRdr nm.lexpr.(e,expr):" ++ (showGhc (e,expr))
        let (FN efs,eds)         = hsFreeAndDeclaredRdr nm expr
        let (FN _eefs,DN eeds) = hsFreeAndDeclaredRdr nm e
        -- logm $ "hsVisibleDsRdr nm.lexpr (efs,_d,_eefs,eeds) " ++ show (efs,_d,_eefs,eeds)
        return (DN (efs \\ eeds) <> eds)

    lexpr _x = do
      -- logm $ "hsVisibleDsRdr.lexpr:miss for:" ++ SYB.showData SYB.Parser 0 x
      return (DN [])

    -- ---------------------------------

    tyclgroups :: [GHC.TyClGroup GhcPs] -> RefactGhc DeclaredNames
    tyclgroups tgrps
      | findNameInRdr nm e tgrps = do
        fds <- mapM (hsVisibleDsRdr nm e) tgrps
        return $ mconcat fds
    tyclgroups _ = return (DN [])

    tyclgroup :: GHC.TyClGroup GhcPs -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    tyclgroup (GHC.TyClGroup _ tyclds _roles instds)
      | findNameInRdr nm e instds = do
        fds <- mapM (hsVisibleDsRdr nm e) instds
        return $ mconcat fds
      | findNameInRdr nm e tyclds = do
        fds <- mapM (hsVisibleDsRdr nm e) tyclds
        return $ mconcat fds
#elif __GLASGOW_HASKELL__ > 800
    tyclgroup (GHC.TyClGroup tyclds _roles instds)
      | findNameInRdr nm e instds = do
        fds <- mapM (hsVisibleDsRdr nm e) instds
        return $ mconcat fds
      | findNameInRdr nm e tyclds = do
        fds <- mapM (hsVisibleDsRdr nm e) tyclds
        return $ mconcat fds
#else
    tyclgroup (GHC.TyClGroup tyclds _roles)
      | findNameInRdr nm e tyclds = do
        fds <- mapM (hsVisibleDsRdr nm e) tyclds
        return $ mconcat fds
#endif
    tyclgroup _ = return (DN [])

    tycldeclss :: [[GHC.LTyClDecl GhcPs]] -> RefactGhc DeclaredNames
    tycldeclss tcds
      | findNameInRdr nm e tcds = do
        fds <- mapM (hsVisibleDsRdr nm e) tcds
        return $ mconcat fds
    tycldeclss _ = return (DN [])

    tycldecls :: [GHC.LTyClDecl GhcPs] -> RefactGhc DeclaredNames
    tycldecls tcds
      | findNameInRdr nm e tcds = do
        fds <- mapM (hsVisibleDsRdr nm e) tcds
        return $ mconcat fds
    tycldecls _ = return (DN [])

    ltycldecl :: GHC.LTyClDecl GhcPs -> RefactGhc DeclaredNames
    ltycldecl tcd
      | findNameInRdr nm e tcd = do
        let (_,ds) = hsFreeAndDeclaredRdr nm tcd
        return ds
    ltycldecl _ = return (DN [])

    tycldecl :: GHC.TyClDecl GhcPs -> RefactGhc DeclaredNames
    tycldecl tcd
      | findNameInRdr nm e tcd = do
        let (_,ds) = hsFreeAndDeclaredRdr nm tcd
        return ds
    tycldecl _ = return (DN [])

    hsdatadefn :: GHC.HsDataDefn GhcPs -> RefactGhc DeclaredNames
    hsdatadefn tcd
      | findNameInRdr nm e tcd = do
        let (_,ds) = hsFreeAndDeclaredRdr nm tcd
        return ds
    hsdatadefn _ = return (DN [])

    condecl :: GHC.ConDecl GhcPs -> RefactGhc DeclaredNames
    condecl tcd
      | findNameInRdr nm e tcd = do
        let (_,ds) = hsFreeAndDeclaredRdr nm tcd
        return ds
    condecl _ = return (DN [])

    -- ---------------------------------

    instdecls :: [GHC.LInstDecl GhcPs] -> RefactGhc DeclaredNames
    instdecls ds
      | findNameInRdr nm e ds = do
        fds <- mapM (hsVisibleDsRdr nm e) ds
        return $ mconcat fds
    instdecls _ = return (DN [])

    instdecl :: GHC.LInstDecl GhcPs -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    instdecl (GHC.L _ (GHC.ClsInstD _ (GHC.ClsInstDecl _ polytyp binds sigs tyfaminsts dfaminsts _)))
#else
    instdecl (GHC.L _ (GHC.ClsInstD (GHC.ClsInstDecl polytyp binds sigs tyfaminsts dfaminsts _)))
#endif
      | findNameInRdr nm e polytyp    = hsVisibleDsRdr nm e polytyp
      | findNameInRdr nm e binds      = hsVisibleDsRdr nm e binds
      | findNameInRdr nm e sigs       = hsVisibleDsRdr nm e sigs
      | findNameInRdr nm e tyfaminsts = hsVisibleDsRdr nm e tyfaminsts
      | findNameInRdr nm e dfaminsts  = hsVisibleDsRdr nm e dfaminsts
      | otherwise = return (DN [])
#if __GLASGOW_HASKELL__ >= 808
    instdecl (GHC.L _ (GHC.DataFamInstD _ (GHC.DataFamInstDecl (GHC.HsIB _ (GHC.FamEqn _ _ln _bndrs pats _fixity defn ) ))))
#elif __GLASGOW_HASKELL__ >= 806
    instdecl (GHC.L _ (GHC.DataFamInstD _ (GHC.DataFamInstDecl (GHC.HsIB _ (GHC.FamEqn _ _ln pats _fixity defn ) ))))
#elif __GLASGOW_HASKELL__ >= 804
    instdecl (GHC.L _ (GHC.DataFamInstD (GHC.DataFamInstDecl (GHC.HsIB _ (GHC.FamEqn _ln pats _fixity defn ) _ ))))
#elif __GLASGOW_HASKELL__ > 800
    instdecl (GHC.L _ (GHC.DataFamInstD (GHC.DataFamInstDecl _ln pats _fixity defn _)))
#else
    instdecl (GHC.L _ (GHC.DataFamInstD (GHC.DataFamInstDecl _ln pats defn _)))
#endif
      | findNameInRdr nm e pats = hsVisibleDsRdr nm e pats
      | findNameInRdr nm e defn = hsVisibleDsRdr nm e defn
      | otherwise = return (DN [])
#if __GLASGOW_HASKELL__ >= 806
    instdecl (GHC.L _ (GHC.TyFamInstD _ (GHC.TyFamInstDecl eqn)))
#elif __GLASGOW_HASKELL__ >= 804
    instdecl (GHC.L _ (GHC.TyFamInstD (GHC.TyFamInstDecl eqn)))
#else
    instdecl (GHC.L _ (GHC.TyFamInstD (GHC.TyFamInstDecl eqn _)))
#endif
      | findNameInRdr nm e eqn = hsVisibleDsRdr nm e eqn
      | otherwise = return (DN [])
#if __GLASGOW_HASKELL__ >= 806
    instdecl (GHC.L _ (GHC.ClsInstD _ (GHC.XClsInstDecl {}))) = mzero
    instdecl (GHC.L _ (GHC.DataFamInstD _ (GHC.DataFamInstDecl (GHC.HsIB _ (GHC.XFamEqn _))))) = mzero
    instdecl (GHC.L _ (GHC.DataFamInstD _ (GHC.DataFamInstDecl (GHC.XHsImplicitBndrs  _))))    = mzero
    instdecl (GHC.L _ (GHC.XInstDecl _)) = mzero
#endif

    lhstype :: GHC.LHsType GhcPs -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    lhstype tv@(GHC.L l (GHC.HsTyVar _ _ (GHC.L _ n)))
#elif __GLASGOW_HASKELL__ > 800
    lhstype tv@(GHC.L l (GHC.HsTyVar _ (GHC.L _ n)))
#elif __GLASGOW_HASKELL__ > 710
    lhstype tv@(GHC.L l (GHC.HsTyVar (GHC.L _ n)))
#else
    lhstype tv@(GHC.L l (GHC.HsTyVar n))
#endif
      | findNameInRdr nm e tv = return (DN [rdrName2NamePure nm (GHC.L l n)])
      | otherwise       = return (DN [])
    lhstype (GHC.L _ (GHC.HsForAllTy {}))
        = return (DN [])
    lhstype (GHC.L _ (GHC.HsFunTy{})) = return (DN [])
    lhstype _ty = do
      -- logm $ "lshtype: TypeUtils 1588" ++ SYB.showData SYB.Renamer 0 ty
      return (DN [])

    -- -----------------------

    lsigs :: [GHC.LSig GhcPs] -> RefactGhc DeclaredNames
    lsigs ss = do
      fds <- mapM (hsVisibleDsRdr nm e) ss
      return $ mconcat fds

    -- -----------------------

    lsig :: GHC.LSig GhcPs -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    lsig (GHC.L _ (GHC.TypeSig _ _ns typ))
#elif __GLASGOW_HASKELL__ > 710
    lsig (GHC.L _ (GHC.TypeSig _ns typ))
#else
    lsig (GHC.L _ (GHC.TypeSig _ns typ _))
#endif
      | findNameInRdr nm e typ = hsVisibleDsRdr nm e typ
#if __GLASGOW_HASKELL__ >= 806
    lsig (GHC.L _ (GHC.ClassOpSig _ _ _n (GHC.HsIB _ typ)))
#elif __GLASGOW_HASKELL__ > 800
    lsig (GHC.L _ (GHC.ClassOpSig _ _n (GHC.HsIB _ typ _)))
#elif __GLASGOW_HASKELL__ > 710
    lsig (GHC.L _ (GHC.ClassOpSig _ _n (GHC.HsIB _ typ)))
#else
    lsig (GHC.L _ (GHC.GenericSig _n typ))
#endif
      | findNameInRdr nm e typ = hsVisibleDsRdr nm e typ
    lsig (GHC.L _ (GHC.IdSig {})) = return (DN [])
    lsig (GHC.L _ (GHC.InlineSig {})) = return (DN [])
#if __GLASGOW_HASKELL__ >= 806
    lsig (GHC.L _ (GHC.SpecSig _ _n typ _))
#else
    lsig (GHC.L _ (GHC.SpecSig _n typ _))
#endif
      | findNameInRdr nm e typ = hsVisibleDsRdr nm e typ
    lsig (GHC.L _ (GHC.SpecInstSig {})) = return (DN [])

    lsig _ = return (DN [])

    -- -----------------------

    lstmts :: [GHC.LStmt GhcPs (GHC.LHsExpr GhcPs)] -> RefactGhc DeclaredNames
    lstmts ds
      | findNameInRdr nm e ds = do
        fds <- mapM (hsVisibleDsRdr nm e) ds
        return $ mconcat fds
    lstmts _ = return (DN [])

    -- -----------------------

    lstmt :: GHC.LStmt GhcPs (GHC.LHsExpr GhcPs) -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    lstmt (GHC.L _ (GHC.LastStmt _ ex _ _)) = hsVisibleDsRdr nm e ex
#elif __GLASGOW_HASKELL__ > 710
    lstmt (GHC.L _ (GHC.LastStmt ex _ _)) = hsVisibleDsRdr nm e ex
#else
    lstmt (GHC.L _ (GHC.LastStmt ex _)) = hsVisibleDsRdr nm e ex
#endif
#if __GLASGOW_HASKELL__ >= 806
    lstmt (GHC.L _ (GHC.BindStmt _ pa ex _ _)) = do
#elif __GLASGOW_HASKELL__ > 710
    lstmt (GHC.L _ (GHC.BindStmt pa ex _ _ _)) = do
#else
    lstmt (GHC.L _ (GHC.BindStmt pa ex _ _)) = do
#endif
      fdp <- hsVisibleDsRdr nm e pa
      fde <- hsVisibleDsRdr nm e ex
      return (fdp <> fde)
#if __GLASGOW_HASKELL__ >= 806
    lstmt (GHC.L _ (GHC.BodyStmt _ ex _ _)) = hsVisibleDsRdr nm e ex
#else
    lstmt (GHC.L _ (GHC.BodyStmt ex _ _ _)) = hsVisibleDsRdr nm e ex
#endif

#if __GLASGOW_HASKELL__ >= 806
    lstmt (GHC.L _ (GHC.LetStmt _ bs)) = hsVisibleDsRdr nm e bs
#else
    lstmt (GHC.L _ (GHC.LetStmt bs)) = hsVisibleDsRdr nm e bs
#endif
#if __GLASGOW_HASKELL__ <= 710
    lstmt (GHC.L _ (GHC.ParStmt ps _ _)) = hsVisibleDsRdr nm e ps
#else
    lstmt (GHC.L _ (GHC.ParStmt ps _ _ _)) = hsVisibleDsRdr nm e ps
#endif
#if __GLASGOW_HASKELL__ >= 806
    lstmt (GHC.L _ (GHC.TransStmt _ _ stmts _ using mby _ _ _)) = do
#elif __GLASGOW_HASKELL__ > 710
    lstmt (GHC.L _ (GHC.TransStmt _ stmts _ using mby _ _ _ _)) = do
#else
    lstmt (GHC.L _ (GHC.TransStmt _ stmts _ using mby _ _ _)) = do
#endif
      fds <- hsVisibleDsRdr nm e stmts
      fdu <- hsVisibleDsRdr nm e using
      fdb <- case mby of
        Nothing -> return (DN [])
        Just ex -> hsVisibleDsRdr nm e ex
      return $ fds <> fdu <> fdb
#if __GLASGOW_HASKELL__ >= 806
    lstmt (GHC.L _ (GHC.RecStmt _ stmts _ _ _ _ _)) = hsVisibleDsRdr nm e stmts
#elif __GLASGOW_HASKELL__ > 710
    lstmt (GHC.L _ (GHC.RecStmt stmts _ _ _ _ _ _ _ _ _)) = hsVisibleDsRdr nm e stmts
#else
    lstmt (GHC.L _ (GHC.RecStmt stmts _ _ _ _ _ _ _ _)) = hsVisibleDsRdr nm e stmts
#endif

#if __GLASGOW_HASKELL__ > 710
    lstmt (GHC.L _ (GHC.ApplicativeStmt {})) = return mempty
#endif
#if __GLASGOW_HASKELL__ >= 806
    lstmt (GHC.L _ (GHC.XStmtLR _)) = mzero
#endif
    -- lstmt _ = return (DN [])

    -- -----------------------

    lpats :: [GHC.LPat GhcPs] -> RefactGhc DeclaredNames
    lpats ps
      | findNameInRdr nm e ps = do
        fds <- mapM (hsVisibleDsRdr nm e) ps
        return $ mconcat fds
    lpats _ = return (DN [])

    -- -----------------------

    lpat :: GHC.LPat GhcPs -> RefactGhc DeclaredNames
    lpat (LL _ (GHC.WildPat _)) = return (DN [])
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL l (GHC.VarPat _ (GHC.L _ n)))
#elif __GLASGOW_HASKELL__ > 710
    lpat (GHC.L l (GHC.VarPat (GHC.L _ n)))
#else
    lpat (GHC.L l (GHC.VarPat n))
#endif
      = return (DN [rdrName2NamePure nm (GHC.L l n)])
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL _ (GHC.AsPat _ ln p)) = do
#else
    lpat (GHC.L _ (GHC.AsPat ln p)) = do
#endif
      (DN dp) <- lpat p
      return (DN (rdrName2NamePure nm ln:dp))

#if __GLASGOW_HASKELL__ >= 806
    lpat (LL _ (GHC.ParPat _ p)) = lpat p
    lpat (LL _ (GHC.BangPat _ p)) = lpat p
#else
    lpat (GHC.L _ (GHC.ParPat p)) = lpat p
    lpat (GHC.L _ (GHC.BangPat p)) = lpat p
#endif
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL _ (GHC.ListPat _ ps)) = do
#else
    lpat (GHC.L _ (GHC.ListPat ps _ _)) = do
#endif
      fds <- mapM lpat ps
      return $ mconcat fds
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL _ (GHC.TuplePat _ ps _)) = do
#else
    lpat (GHC.L _ (GHC.TuplePat ps _ _)) = do
#endif
      fds <- mapM lpat ps
      return $ mconcat fds
#if __GLASGOW_HASKELL__ < 806
    lpat (GHC.L _ (GHC.PArrPat ps _)) = do
      fds <- mapM lpat ps
      return $ mconcat fds
#endif
    lpat (LL _ (GHC.ConPatIn n det)) = do
      (DN d) <- details det
      return $ (DN (rdrName2NamePure nm n:d))
    -- lpat (GHC.ConPatOut )
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL _ (GHC.ViewPat _ ex p)) = do
#else
    lpat (GHC.L _ (GHC.ViewPat ex p _)) = do
#endif
      fde <- hsVisibleDsRdr nm e ex
      fdp <- lpat p
      return $ fde <> fdp
    -- lpat (GHC.QuasiQuotePat _)
    lpat (LL _ (GHC.LitPat {})) = return (DN [])
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL _ (GHC.NPat {})) = return (DN [])
    lpat (LL _ (GHC.NPlusKPat _ n _ _ _ _)) = return (DN [rdrName2NamePure nm n])
#elif __GLASGOW_HASKELL__ > 710
    lpat (GHC.L _ (GHC.NPat _ _ _ _)) = return (DN [])
    lpat (GHC.L _ (GHC.NPlusKPat n _ _ _ _ _)) = return (DN [rdrName2NamePure nm n])
#else
    lpat (GHC.L _ (GHC.NPat _ _ _)) = return (DN [])
    lpat (GHC.L _ (GHC.NPlusKPat n _ _ _)) = return (DN [rdrName2NamePure nm n])
#endif
#if __GLASGOW_HASKELL__ >= 808
    lpat (LL _ _p@(GHC.SigPat _ p b)) = do
      dp <- lpat p
      db <- hsVisibleDsRdr nm e b
      -- error $ "lpat.SigPatIn:(b,fb,db)" ++ showGhc (b,fb,db)
      return $ dp <> db
#elif __GLASGOW_HASKELL__ >= 806
    lpat (GHC.L _ _p@(GHC.SigPat b p)) = do
      dp <- lpat p
      db <- hsVisibleDsRdr nm e b
      -- error $ "lpat.SigPatIn:(b,fb,db)" ++ showGhc (b,fb,db)
      return $ dp <> db
#else
    lpat (GHC.L _ _p@(GHC.SigPatIn p b)) = do
      dp <- lpat p
      db <- hsVisibleDsRdr nm e b
      -- error $ "lpat.SigPatIn:(b,fb,db)" ++ showGhc (b,fb,db)
      return $ dp <> db
    lpat (GHC.L _ (GHC.SigPatOut p _)) = lpat p
#endif
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL l (GHC.CoPat _ _ p _)) = lpat (LL l p)
    lpat (LL _ (GHC.LazyPat _ p)) = lpat p
#else
    lpat (GHC.L l (GHC.CoPat _ p _)) = lpat (GHC.L l p)
    lpat (GHC.L _ (GHC.LazyPat p)) = lpat p
#endif

    lpat (LL _ (GHC.ConPatOut {})) = error $ "hsFreeAndDeclared.lpat:impossible GHC.ConPatOut"
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL _ (GHC.SplicePat _ (GHC.HsTypedSplice _ _ _ expr)))   = hsVisibleDsRdr nm e expr
    lpat (LL _ (GHC.SplicePat _ (GHC.HsUntypedSplice _ _ _ expr))) = hsVisibleDsRdr nm e expr
    lpat (LL _ (GHC.SplicePat _ (GHC.HsSpliced {})))           = return mempty
    lpat (LL _ (GHC.SplicePat _ (GHC.HsQuasiQuote {})))        = return mempty
    lpat (LL _ (GHC.SplicePat _ (GHC.XSplice {})))             = return mempty
#elif __GLASGOW_HASKELL__ > 800
    lpat (GHC.L _ (GHC.SplicePat (GHC.HsTypedSplice _ _ expr)))   = hsVisibleDsRdr nm e expr
    lpat (GHC.L _ (GHC.SplicePat (GHC.HsUntypedSplice _ _ expr))) = hsVisibleDsRdr nm e expr
    lpat (GHC.L _ (GHC.SplicePat (GHC.HsSpliced _ _))) = return mempty
    lpat (GHC.L _ (GHC.SplicePat (GHC.HsQuasiQuote {})))        = return mempty
#elif __GLASGOW_HASKELL__ > 710
    lpat (GHC.L _ (GHC.SplicePat (GHC.HsTypedSplice _ expr)))   = hsVisibleDsRdr nm e expr
    lpat (GHC.L _ (GHC.SplicePat (GHC.HsUntypedSplice _ expr))) = hsVisibleDsRdr nm e expr
    lpat (GHC.L _ (GHC.SplicePat (GHC.HsQuasiQuote {})))        = return mempty
#else
    lpat (GHC.L _ (GHC.QuasiQuotePat _)) = return mempty
    lpat (GHC.L _ (GHC.SplicePat (GHC.HsSplice _ expr))) = hsVisibleDsRdr nm e expr
#endif
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL _ (GHC.SumPat _ p _ _)) = lpat p
#elif __GLASGOW_HASKELL__ > 800
    lpat (GHC.L _ (GHC.SumPat p _ _ _)) = lpat p
#endif
#if __GLASGOW_HASKELL__ >= 806
    lpat (LL _ (GHC.XPat _)) = mzero
#endif

    -- ---------------------------

    details :: GHC.HsConPatDetails GhcPs -> RefactGhc DeclaredNames
    details (GHC.PrefixCon  args) = do
      fds <- mapM lpat args
      return $ mconcat fds
    details (GHC.RecCon recf) =
      recfields recf
    details (GHC.InfixCon arg1 arg2) = do
      fds <- mapM lpat [arg1,arg2]
      return $ mconcat fds

    -- Note: this one applies to HsRecFields in LPats
    recfields :: (GHC.HsRecFields GhcPs (GHC.LPat GhcPs)) -> RefactGhc DeclaredNames
    recfields (GHC.HsRecFields fields _) = do
      let args = map (\(GHC.L _ (GHC.HsRecField _ arg _)) -> arg) fields
      fds <- mapM lpat args
      return $ mconcat fds

    -- -----------------------
#if __GLASGOW_HASKELL__ >= 806
    ibndrs :: GHC.LHsSigWcType GhcPs -> RefactGhc DeclaredNames
    ibndrs (GHC.HsWC _ (GHC.HsIB _ ty)) = hsVisibleDsRdr nm e ty
    ibndrs (GHC.HsWC _ (GHC.XHsImplicitBndrs _)) = mzero
    ibndrs (GHC.XHsWildCardBndrs _) = mzero

    lsigty :: GHC.LHsSigType GhcPs -> RefactGhc DeclaredNames
    lsigty (GHC.HsIB _ ty) = hsVisibleDsRdr nm e ty
    lsigty (GHC.XHsImplicitBndrs _) = mzero
#elif __GLASGOW_HASKELL__ > 800
    ibndrs :: GHC.LHsSigWcType GhcPs -> RefactGhc DeclaredNames
    ibndrs (GHC.HsWC _ (GHC.HsIB _ ty _)) = hsVisibleDsRdr nm e ty

    lsigty :: GHC.LHsSigType GhcPs -> RefactGhc DeclaredNames
    lsigty (GHC.HsIB _ ty _) = hsVisibleDsRdr nm e ty
#elif __GLASGOW_HASKELL__ > 710
    ibndrs :: GHC.LHsSigWcType GhcPs -> RefactGhc DeclaredNames
    ibndrs (GHC.HsIB _ (GHC.HsWC _ _ ty)) = hsVisibleDsRdr nm e ty

    lsigty :: GHC.LHsSigType GhcPs -> RefactGhc DeclaredNames
    lsigty (GHC.HsIB _ ty) = hsVisibleDsRdr nm e ty
#endif
    -- -----------------------
    lanndecl :: GHC.LAnnDecl GhcPs -> RefactGhc DeclaredNames
#if __GLASGOW_HASKELL__ >= 806
    lanndecl (GHC.L _ (GHC.HsAnnotation _ _ _ expr)) = hsVisibleDsRdr nm e expr
    lanndecl (GHC.L _ (GHC.XAnnDecl _)) = mzero
#else
    lanndecl (GHC.L _ (GHC.HsAnnotation _ _ expr)) = hsVisibleDsRdr nm e expr
#endif
    -- -----------------------

    err = error $ "hsVisibleDsRdr nm:no match for:" ++ (showAnnData mempty 0 t)

-- ---------------------------------------------------------------------

-- |`hsFDsFromInsideRdr` is different from `hsFreeAndDeclaredPNs` in
-- that: given an syntax phrase t, `hsFDsFromInsideRdr` returns not only
-- the declared variables that are visible from outside of t, but also
-- those declared variables that are visible to the main expression
-- inside t.
-- NOTE: Expects to be given ParsedSource
hsFDsFromInsideRdr :: (SYB.Data t)
                  => NameMap ->  t -> (FreeNames,DeclaredNames)
hsFDsFromInsideRdr nm t = hsFDsFromInsideRdr' t
   where
     hsFDsFromInsideRdr' :: (SYB.Data t) => t -> (FreeNames,DeclaredNames)
     hsFDsFromInsideRdr' t1 = (FN $ nub f', DN $ nub d')
       where
          r1 = applyTU (once_tdTU (failTU  `adhocTU` parsed
                                           `adhocTU` decl
                                           `adhocTU` match
                                           `adhocTU` expr
                                           `adhocTU` stmts )) t1
          -- let (f',d') = fromMaybe ([],[]) r1
          (FN f',DN d') = fromMaybe (FN [],DN []) r1
          -- (FN f',DN d') = r1

     parsed :: GHC.ParsedSource -> Maybe (FreeNames,DeclaredNames)
     parsed p = return $ hsFreeAndDeclaredRdr nm p

     -- ----------------------

     match :: GHC.Match GhcPs (GHC.LHsExpr GhcPs) -> Maybe (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
     match (GHC.Match _ _fn pats rhs) = do
#elif __GLASGOW_HASKELL__ >= 804
     match (GHC.Match _fn pats rhs) = do
#else
     match (GHC.Match _fn pats _type rhs) = do
#endif
       let (FN pf, DN pd) = hsFreeAndDeclaredRdr nm pats
           (FN rf, DN rd) = hsFreeAndDeclaredRdr nm rhs
       return (FN $ nub (pf `union` (rf \\ pd)),
               DN $ nub (pd `union` rd))
#if __GLASGOW_HASKELL__ >= 806
     match (GHC.XMatch _) = mzero
#endif

     -- ----------------------

     decl :: GHC.HsBind GhcPs -> Maybe (FreeNames,DeclaredNames)
#if __GLASGOW_HASKELL__ >= 806
     decl (GHC.FunBind _ (GHC.L _ _) (GHC.MG _ (GHC.L _ matches) _) _ _) = do
#elif __GLASGOW_HASKELL__ > 710
     decl (GHC.FunBind (GHC.L _ _) (GHC.MG (GHC.L _ matches) _ _ _) _ _ _) = do
#else
     decl (GHC.FunBind (GHC.L _ _) _ (GHC.MG matches _ _ _) _ _ _) = do
#endif
       let
         fds = map hsFDsFromInsideRdr' matches
         -- error (show $ nameToString n)
       return (FN $ nub (concat $ map (fn . fst) fds), DN $ nub (concat $ map (dn . snd) fds))

#if __GLASGOW_HASKELL__ >= 806
     decl ((GHC.PatBind _ p rhs _) :: GHC.HsBind GhcPs) = do
#else
     decl ((GHC.PatBind p rhs _ _ _) :: GHC.HsBind GhcPs) = do
#endif
       let
         (FN pf, DN pd) = hsFreeAndDeclaredRdr nm p
         (FN rf, DN rd) = hsFreeAndDeclaredRdr nm rhs
       return
           (FN $ nub (pf `union` (rf \\ pd)),
            DN $ nub (pd `union` rd))

#if __GLASGOW_HASKELL__ >= 806
     decl ((GHC.VarBind _ p rhs _) :: GHC.HsBind GhcPs) = do
#else
     decl ((GHC.VarBind p rhs _) :: GHC.HsBind GhcPs) = do
#endif
       let
         (FN pf, DN pd) = hsFreeAndDeclaredRdr nm p
         (FN rf, DN rd) = hsFreeAndDeclaredRdr nm rhs
       return
           (FN $ nub (pf `union` (rf \\ pd)),
            DN $ nub (pd `union` rd))

     decl _ = mzero

     -- ----------------------

#if __GLASGOW_HASKELL__ >= 806
     expr ((GHC.HsLet _ decls e) :: GHC.HsExpr GhcPs) = do
#else
     expr ((GHC.HsLet decls e) :: GHC.HsExpr GhcPs) = do
#endif
       let
         (FN df,DN dd) = hsFreeAndDeclaredRdr nm decls
         (FN ef,_)     = hsFreeAndDeclaredRdr nm e
       return (FN $ nub (df `union` (ef \\ dd)), DN $ nub dd)

#if __GLASGOW_HASKELL__ >= 806
     expr ((GHC.HsLam _ (GHC.MG _ matches _)) :: GHC.HsExpr GhcPs) =
#else
     expr ((GHC.HsLam (GHC.MG matches _ _ _)) :: GHC.HsExpr GhcPs) =
#endif
       return $ hsFreeAndDeclaredRdr nm matches

#if __GLASGOW_HASKELL__ >= 806
     expr ((GHC.HsCase _ e (GHC.MG _ matches _)) :: GHC.HsExpr GhcPs) = do
#else
     expr ((GHC.HsCase e (GHC.MG matches _ _ _)) :: GHC.HsExpr GhcPs) = do
#endif
       let
         (FN ef,_)     = hsFreeAndDeclaredRdr nm e
         (FN df,DN dd) = hsFreeAndDeclaredRdr nm matches
       return (FN $ nub (df `union` (ef \\ dd)), DN $ nub dd)

     expr _ = return (FN [],DN [])

#if __GLASGOW_HASKELL__ >= 806
     stmts ((GHC.BindStmt _ pat e1 e2 e3 ) :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) = do
#elif __GLASGOW_HASKELL__ > 710
     stmts ((GHC.BindStmt pat e1 e2 e3 _) :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) = do
#else
     stmts ((GHC.BindStmt pat e1 e2 e3) :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) = do
#endif
       let
         (FN pf,DN pd)  = hsFreeAndDeclaredRdr nm pat
         (FN ef,DN _ed) = hsFreeAndDeclaredRdr nm e1
         (FN df,DN dd)  = hsFreeAndDeclaredRdr nm [e2,e3]
       return
           (FN $ nub (pf `union` (((ef \\ dd) `union` df) \\ pd)), DN $ nub (pd `union` dd))

#if __GLASGOW_HASKELL__ >= 806
     stmts ((GHC.LetStmt _ binds) :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) =
#else
     stmts ((GHC.LetStmt binds) :: GHC.Stmt GhcPs (GHC.LHsExpr GhcPs)) =
#endif
       return $ hsFreeAndDeclaredRdr nm binds

     stmts _ = return (FN [],DN [])


-- ---------------------------------------------------------------------

-- | The same as `hsFDsFromInside` except that the returned variables
-- are in the String format
hsFDNamesFromInsideRdr ::(SYB.Data t) => t -> RefactGhc ([String],[String])
hsFDNamesFromInsideRdr t = do
  nm <- getRefactNameMap
  return (hsFDNamesFromInsideRdrPure nm t)
  -- (FN f,DN d) <- hsFDsFromInsideRdr nm t
  -- return ((nub.map showGhc) f, (nub.map showGhc) d)

-- | The same as `hsFDsFromInside` except that the returned variables
-- are in the String format
hsFDNamesFromInsideRdrPure :: (SYB.Data t) => NameMap -> t -> ([String],[String])
hsFDNamesFromInsideRdrPure nm t = ((nub.map showGhc) f, (nub.map showGhc) d)
  where
    (FN f,DN d) = hsFDsFromInsideRdr nm t

-- ---------------------------------------------------------------------

rdrName2Name :: GHC.Located GHC.RdrName -> RefactGhc GHC.Name
rdrName2Name ln = do
  nameMap <- getRefactNameMap
  return (rdrName2NamePure nameMap ln)

rdrName2NamePure :: NameMap -> GHC.Located GHC.RdrName -> GHC.Name
rdrName2NamePure _nameMap (GHC.L _ (GHC.Exact n)) = n
rdrName2NamePure nameMap (GHC.L lrn _) =
  fromMaybe (error $ "rdrName2NamePure: no name found for" ++ showGhc lrn)
  -- fromMaybe (error $ "rdrName2NamePure: no name found for (lrn,e,nameMap)=" ++ showGhc (lrn,e,nameMap))
             (Map.lookup lrn nameMap)

eqRdrNamePure :: NameMap -> GHC.Located GHC.RdrName -> GHC.Name -> Bool
eqRdrNamePure nameMap rn n
  = GHC.nameUnique (rdrName2NamePure nameMap rn) == GHC.nameUnique n

-- ---------------------------------------------------------------------

-- | Returns True if both @GHC.Name@s are in the same @GHC.NameSpace@.
sameNameSpace :: GHC.Name -> GHC.Name -> Bool
sameNameSpace n1 n2
  = (GHC.occNameSpace $ GHC.nameOccName n1) == (GHC.occNameSpace $ GHC.nameOccName n2)

-- ---------------------------------------------------------------------

-- |Find the identifier(in GHC.Name format) whose start position is
-- (row,col) in the file specified by the fileName, and returns
-- `Nothing` if such an identifier does not exist.
locToName :: (SYB.Data t)
                     => SimpPos          -- ^ The row and column number
                     -> t                -- ^ The syntax phrase, parameterised by RdrName
                     -> RefactGhc (Maybe GHC.Name)  -- ^ The result
locToName pos t = do
   nm <- getRefactNameMap
   let mn = locToRdrName pos t
   return $ fmap (rdrName2NamePure nm) mn

-- |Find the identifier(in GHC.Name format) whose start position is
-- (row,col) in the file specified by the fileName, and returns
-- `Nothing` if such an identifier does not exist.
locToNamePure :: (SYB.Data t)
                    => NameMap
                    -> SimpPos         -- ^ The row and column number
                    -> t               -- ^ The syntax phrase, parameterised by RdrName
                    -> Maybe GHC.Name  -- ^ The result
locToNamePure nm pos t =
  let mn = locToRdrName pos t
  in fmap (rdrName2NamePure nm) mn

-- |Find the identifier(in GHC.RdrName format) whose start position is
-- (row,col) in the file specified by the fileName, and returns
-- `Nothing` if such an identifier does not exist.
locToRdrName::(SYB.Data t)
                    =>SimpPos          -- ^ The row and column number
                    ->t                -- ^ The syntax phrase
                    -> Maybe (GHC.Located GHC.RdrName)  -- ^ The result
locToRdrName (row,col) t = locToName' (row,col) t



locToId :: (SYB.Data t) => SimpPos -> t -> Maybe (GHC.Located GHC.Id)
locToId (row,col) t = locToName' (row,col) t


-- |Worker for both locToName and locToRdrName.
-- NOTE: provides for FunBind MatchGroups where only the first name is
-- retained in the AST

locToName':: forall a t.(SYB.Data t, SYB.Data a)
                    =>SimpPos          -- ^ The row and column number
                    ->t                -- ^ The syntax phrase
                    -> Maybe (GHC.Located a)  -- ^ The result
locToName' (row,col) t = res1
     where
        res1 :: Maybe (GHC.Located a)
        res1 = SYB.something (nameSybQuery checker) t

        checker pnt =
          -- trace ("locToName':pnt=" ++ show (GHC.getLoc pnt)) $
          if inScope pnt
             then Just pnt
             else Nothing

        -- ++AZ++:TODO: Is inScope actually required?
        inScope :: GHC.Located e -> Bool
        inScope (GHC.L l _) =
          case l of
            (GHC.UnhelpfulSpan _) -> False
            (GHC.RealSrcSpan ss)  ->
              -- (GHC.srcSpanFile ss == fileName) &&
              (GHC.srcSpanStartLine ss <= row) &&
              (GHC.srcSpanEndLine ss   >= row) &&
              (col >= (GHC.srcSpanStartCol ss)) &&
              (col <= (GHC.srcSpanEndCol   ss))

-- EOF
