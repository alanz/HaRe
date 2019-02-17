{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.Refact.Utils.Monad
       (
         ParseResult
       , VerboseLevel(..)
       , RefactSettings(..)
       , RefactState(..)
       , RefactModule(..)
       , RefacSource(..)
       , TargetModule
       , Targets
       , CabalGraph
       , RefactStashId(..)
       , RefactFlags(..)
       , StateStorage(..)

       -- The GHC Monad
       , RefactGhc(..)
       , runRefactGhc
       , getRefacSettings
       , defaultSettings
       , logSettings

       , cabalModuleGraphs
       , canonicalizeGraph
       , canonicalizeModSummary

       , logm
       ) where


import qualified DynFlags      as GHC
import qualified GHC           as GHC
import qualified HscTypes      as GHC
import qualified Outputable    as GHC

import Control.Applicative
#if __GLASGOW_HASKELL__ >= 806
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.State
import Data.IORef
--import Data.Time.Clock
import Distribution.Helper
import Exception
import qualified GhcModCore         as GM (runGhcModT,GhcModT,Options(..),IOish,cradleCabalFile,
                                           cradle,unGmlT,gmlGetSession,gmlSetSession,gmcHomeModuleGraph,GmOut(..),MonadIO(..),GmEnv(..),
                                           cabalResolvedComponents,
                                           ModulePath,GmComponent,GmComponentType(..),GmModuleGraph(..))
import Language.Haskell.Refact.Utils.Types
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import System.Directory
import System.Log.Logger

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Monad transformer stuff
import Control.Monad.Trans.Control ( control, liftBaseOp, liftBaseOp_)

-- ---------------------------------------------------------------------

data VerboseLevel = Debug | Normal | Off
            deriving (Eq,Show)

data RefactSettings = RefSet
        {
        -- , rsetMainFile     :: Maybe [FilePath]
           -- TODO: re-instate rsetMainFile for when there is no cabal
           -- file.
          rsetVerboseLevel :: !VerboseLevel
        , rsetEnabledTargets :: (Bool,Bool,Bool,Bool)
        } deriving (Show)

-- deriving instance Show LineSeparator

defaultSettings :: RefactSettings
defaultSettings = RefSet
    {
      rsetVerboseLevel = Normal
    -- , rsetEnabledTargets = (True,False,True,False)
    , rsetEnabledTargets = (True,True,True,True)
    }

logSettings :: RefactSettings
logSettings = defaultSettings { rsetVerboseLevel = Debug }

-- ---------------------------------------------------------------------

data RefactStashId = Stash !String deriving (Show,Eq,Ord)

data RefactModule = RefMod
        { rsTypecheckedMod  :: !GHC.TypecheckedModule
        , rsNameMap         :: NameMap
          -- ^ Mapping from the names in the ParsedSource to the renamed
          -- versions. Note: No strict mark, can be computed lazily.

          -- ++AZ++ TODO: Once HaRe can rename again, change rsTokenCache to something more approriate. Ditto rsStreamModified
        , rsTokenCache      :: !(TokenCache Anns)  -- ^Token stream for the current module, maybe modified, in SrcSpan tree form
        , rsStreamModified  :: !RefacResult        -- ^current module has updated the AST
        } deriving (Show)

instance Show GHC.Name where
  show n = showGhc n

deriving instance Show (GHC.Located GHC.Token)

data RefactFlags = RefFlags
       { rsDone :: !Bool -- ^Current traversal has already made a change
       } deriving (Show)

-- | State for refactoring a single file. Holds/hides the ghc-exactprint
-- annotations, which get updated transparently at key points.
data RefactState = RefSt
        { rsSettings   :: !RefactSettings -- ^Session level settings
        , rsUniqState  :: !Int -- ^ Current Unique creator value, incremented
                               -- every time it is used
        , rsSrcSpanCol :: !Int -- ^ Current SrcSpan creator value, incremented
                               -- every time it is used
        , rsFlags      :: !RefactFlags -- ^ Flags for controlling generic
                                       -- traversals
        , rsStorage    :: !StateStorage -- ^Temporary storage of values while
                                        -- refactoring takes place
        , rsCurrentTarget :: !(Maybe TargetModule) -- TODO:AZ: push this into rsModule
        , rsModule        :: !(Maybe RefactModule) -- ^The current module being refactored
        } deriving (Show)

instance Show (IORef HookIORefData) where
  show _ = "IORef HookIORefData"

{-
Note [rsSrcSpanCol]
~~~~~~~~~~~~~~~~~~~

The ghc-exactprint annotations are tied to a SrcSpan, and provide
deltas for the spaces between the elements in the source.

As such, the SrcSpan itself is only used as an index into the
annotation database.

When HaRe needs a new SrcSpan, for this, it generates it from this
field, to ensure uniqueness.
-}

data RefacSource = RSFile FilePath
                 | RSTarget TargetModule
                 -- x| RSMod GHC.ModSummary
                 | RSAlreadyLoaded
                 deriving (Show)

type TargetModule = GM.ModulePath -- From ghc-mod

instance GHC.Outputable TargetModule where
  ppr t = GHC.text (show t)


-- The CabalGraph comes directly from ghc-mod
-- type CabalGraph = Map.Map ChComponentName (GM.GmComponent GMCResolved (Set.Set ModulePath))
type CabalGraph = Map.Map ChComponentName (GM.GmComponent 'GM.GMCResolved (Set.Set GM.ModulePath))

type Targets = [Either FilePath GHC.ModuleName]

-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.TypecheckedModule

-- |Provide some temporary storage while the refactoring is taking
-- place
data StateStorage = StorageNone
                  | StorageBind (GHC.LHsBind GhcRn)
                  | StorageSig  (GHC.LSig GhcRn)
                  | StorageBindRdr (GHC.LHsBind GhcPs)
                  | StorageDeclRdr (GHC.LHsDecl GhcPs)
                  | StorageSigRdr  (GHC.LSig GhcPs)


instance Show StateStorage where
  show StorageNone         = "StorageNone"
  show (StorageBind _bind) = "(StorageBind " {- ++ (showGhc bind) -} ++ ")"
  show (StorageSig _sig)   = "(StorageSig " {- ++ (showGhc sig) -} ++ ")"
  show (StorageDeclRdr _bind) = "(StorageDeclRdr " {- ++ (showGhc bind) -} ++ ")"
  show (StorageBindRdr _bind) = "(StorageBindRdr " {- ++ (showGhc bind) -} ++ ")"
  show (StorageSigRdr _sig)   = "(StorageSigRdr " {- ++ (showGhc sig) -} ++ ")"

-- ---------------------------------------------------------------------
-- StateT and GhcT stack

newtype RefactGhc a = RefactGhc
    { unRefactGhc :: GM.GhcModT (StateT RefactState IO) a
    } deriving ( Functor
               , Applicative
               , Alternative
               , Monad
               , MonadPlus
               , MonadIO
               , GM.GmEnv
               , GM.GmOut
               , GM.MonadIO
               , ExceptionMonad
               )

#if __GLASGOW_HASKELL__ >= 806
instance Fail.MonadFail RefactGhc where
  fail = Fail.fail
#endif

-- ---------------------------------------------------------------------

runRefactGhc ::
  RefactGhc a -> RefactState -> GM.Options -> IO (a, RefactState)
runRefactGhc comp initState opt = do
    ((merr,_log),s) <- runStateT (GM.runGhcModT opt (unRefactGhc comp)) initState
    case merr of
      Left err -> error (show err)
      Right a  -> return (a,s)

-- ---------------------------------------------------------------------

instance GM.GmOut (StateT RefactState IO) where
  gmoAsk = lift GM.gmoAsk

instance GM.GmOut IO where
  gmoAsk = GM.gmoAsk

instance GM.MonadIO (StateT RefactState IO) where
  liftIO = liftIO

instance MonadState RefactState RefactGhc where
    get   = RefactGhc (lift $ lift get)
    put s = RefactGhc (lift $ lift (put s))

instance GHC.GhcMonad RefactGhc where
  getSession     = RefactGhc $ GM.unGmlT GM.gmlGetSession
  setSession env = RefactGhc $ GM.unGmlT (GM.gmlSetSession env)


instance GHC.HasDynFlags RefactGhc where
  getDynFlags = GHC.hsc_dflags <$> GHC.getSession

-- ---------------------------------------------------------------------

instance ExceptionMonad (StateT RefactState IO) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

-- ---------------------------------------------------------------------

cabalModuleGraphs :: RefactGhc [GM.GmModuleGraph]
cabalModuleGraphs = RefactGhc doCabalModuleGraphs
  where
    doCabalModuleGraphs :: (GM.IOish m) => GM.GhcModT m [GM.GmModuleGraph]
    doCabalModuleGraphs = do
      crdl <- GM.cradle
      case GM.cradleCabalFile crdl of
        Just _ -> do
          mcs <- GM.cabalResolvedComponents
          let graph = map GM.gmcHomeModuleGraph $ Map.elems mcs
          return graph
        Nothing -> return []

-- ---------------------------------------------------------------------

canonicalizeGraph ::
  [GHC.ModSummary] -> RefactGhc [(Maybe FilePath, GHC.ModSummary)]
canonicalizeGraph graph = do
  mm' <- mapM canonicalizeModSummary graph
  return mm'

canonicalizeModSummary :: (MonadIO m) =>
  GHC.ModSummary -> m (Maybe FilePath, GHC.ModSummary)
canonicalizeModSummary modSum = do
  let modSum'  = (\m -> (GHC.ml_hs_file $ GHC.ms_location m, m)) modSum
      canon ((Just fp),m) = do
        fp' <- canonicalizePath fp
        return $ (Just fp',m)
      canon (Nothing,m)  = return (Nothing,m)

  mm' <- liftIO $ canon modSum'

  return mm'

-- ---------------------------------------------------------------------

getRefacSettings :: RefactGhc RefactSettings
getRefacSettings = do
  s <- get
  return (rsSettings s)

-- ---------------------------------------------------------------------

logm :: String -> RefactGhc ()
logm string = do
  settings <- getRefacSettings
  let loggingOn = (rsetVerboseLevel settings == Debug)
             --     || (rsetVerboseLevel settings == Normal)
  when loggingOn $ do
     -- ts <- liftIO timeStamp
     -- liftIO $ warningM "HaRe" (ts ++ ":" ++ string)
     liftIO $ warningM "HaRe" (string)
  return ()

{-
timeStamp :: IO String
timeStamp = do
  k <- getCurrentTime
  return (show k)
-}

-- ---------------------------------------------------------------------

instance Show GHC.ModSummary where
  show m = show $ GHC.ms_mod m

instance Show GHC.Module where
  show m = GHC.moduleNameString $ GHC.moduleName m

-- ---------------------------------------------------------------------
