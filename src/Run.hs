{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Run (run) where

import Control.Monad.Catch
import Data.Either.Validation (Validation (..))
import Data.Text.Display
import Data.Traversable (for)
import Data.Vector (Vector)
import Effectful
import Effectful.FileSystem.IO (hClose)
import Effectful.Log
import Effectful.Process.Typed (TypedProcess, proc, runProcess_)
import Effectful.Reader.Static
import Relude.Extra.Lens (set)

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Dhall qualified
import Dhall.Map qualified
import Effectful.FileSystem.IO.ByteString qualified

import Archlinux.Alpm (
    AlpmConstraint (..),
    AlpmDbPtr,
    AlpmDepend (..),
    AlpmEvent (..),
    AlpmEventType (..),
    AlpmHandlePtr,
    AlpmPkgName,
    AlpmPkgPtr,
    AlpmPkgfrom (..),
    AlpmPkgreason (..),
    AlpmQuestion (..),
    AlpmSiglevel (..),
    AlpmTransflag (..),
    AlpmVersion (..),
    UpdateResult (..),
 )
import Import

import Archlinux.Alpm qualified as Alpm

default (Text)

instance Eq Version where
    x == y = toAlpmVersion x == toAlpmVersion y

instance Ord Version where
    x `compare` y = toAlpmVersion x `compare` toAlpmVersion y

run
    :: ( FileSystem :> es
       , IOE :> es
       , Log :> es
       , Reader Options :> es
       , TypedProcess :> es
       )
    => Eff es ()
run = do
    fp <- fromMaybe "config.dhall" <$> asks optionsConfig
    let
        subst =
            Dhall.Map.fromList
                <$> sequenceA
                    [ ("BuildT",) <$> Dhall.expected (Dhall.auto @Build)
                    , ("ConfigT",) <$> Dhall.expected (Dhall.auto @Config)
                    , ("DatabaseT",) <$> Dhall.expected (Dhall.auto @Database)
                    , ("PackageT",) <$> Dhall.expected (Dhall.auto @Package)
                    , ("SiglevelCheckT",) <$> Dhall.expected (Dhall.auto @SiglevelCheck)
                    , ("SiglevelTrustT",) <$> Dhall.expected (Dhall.auto @SiglevelTrust)
                    , ("VersionsT",) <$> Dhall.expected (Dhall.auto @Versions)
                    , ("VersionT",) <$> Dhall.expected (Dhall.auto @Version)
                    , --
                      pure ("Database", embedDefault (Dhall.inject @Database))
                    , pure ("Package", embedDefault (Dhall.inject @Package))
                    , pure ("SiglevelCheck", embedDefault (Dhall.inject @SiglevelCheck))
                    , pure ("SiglevelTrust", embedDefault (Dhall.inject @SiglevelTrust))
                    , pure ("Versions", embedDefault (Dhall.inject @Versions))
                    -- , pure ("Version"      , embedDefault (Dhall.inject @Version      ))
                    ]
    subst' <- case subst of
        Failure e -> throwM e
        Success subst' -> pure subst'
    let
        settings = set Dhall.substitutions subst' Dhall.defaultEvaluateSettings
    config <- liftIO (Dhall.inputFileWithSettings settings Dhall.auto fp)
    rootdir <- resolveDir' $ rootDir config
    dbdir <- resolveDir' $ databaseDir config
    ensureDir dbdir
    Alpm.withAlpm (fromAbsDir rootdir) (fromAbsDir dbdir) $ \h ->
        run' rootdir dbdir (packages config) h

run'
    :: (FileSystem :> es, IOE :> es, Log :> es, TypedProcess :> es)
    => Path Abs Dir
    -> Path Abs Dir
    -> Vector Package
    -> AlpmHandlePtr
    -> Eff es ()
run' rootdir dbdir packages h = do
    withEffToIO (ConcUnlift Ephemeral Unlimited) $ \runInIO ->
        Alpm.optionSetEventCb h (runInIO . eventLogger)
    localDb <- Alpm.getLocaldb h
    dbs <- case collectDatabases packages of
        Failure es -> throwM $ ConflictingDatabaseDefinitions es
        Success dbs -> do
            dbs' <- mapM (registerDatabaseGlobal h) dbs
            updateResult <- Alpm.dbUpdate h (HashMap.elems dbs') False
            logTrace_ $ case updateResult of
                DbUpdated -> "Databases were updated"
                DbUpdateSkipped -> "Update of databases was skipped"
                DbUpToDate -> "Databases were up to date"
            pure dbs'
    pkgNames <- Alpm.withTrans h [AlpmTransFlagAlldeps, AlpmTransFlagNeeded] $ do
        -- TODO: Set trans flags from config ?
        -- pkgNames <- forConcurrently packages $ \pkg@(Package {..}) -> do
        pkgNames <- for packages $ \pkg@(Package{..}) -> do
            pkg' <- do
                mpkg <-
                    choiceM
                        [ getSyncPkg h pkg
                        , for build (getBuildPkg h pkg)
                        , getLocalPkg h pkg
                        ]
                case mpkg of
                    Nothing -> throwM $ PackageNotFound pkg
                    Just pkg' -> return pkg'
            orig <- Alpm.pkgGetOrigin pkg'
            logTrace_ $ "Origin of " <> display name <> ": " <> display (show @Text orig)
            res <- case orig of
                AlpmPkgFromFile -> do
                    fn <- Alpm.pkgGetFilename pkg'
                    logInfo_ $ "Adding " <> display (show @Text fn) <> " to transaction"
                    Alpm.pkgLoad h True (toAlpmPackageSiglevels sigcheck sigtrust) fn
                AlpmPkgFromSyncdb -> do
                    -- db <-
                    --     Alpm.pkgGetDb pkg' >>= \case
                    --         Nothing ->
                    --             error
                    --                 . Text.pack
                    --                 $ "SHOULD NEVER HAPPEN: No database for "
                    --                     <> packageNameToString pkg
                    --         Just db -> do
                    --             dbname <- Alpm.dbGetName db
                    --             case HashMap.lookup (Text.pack dbname) dbs of
                    --                 Nothing ->
                    --                     error . Text.pack $ "SHOULD NEVER HAPPEN: Database " <> dbname <> " not found"
                    --                 Just db' -> return db'
                    -- n <- Alpm.pkgGetName pkg'
                    -- logInfo_ $ "Adding " <> display (show @Text n) <> " to transaction"
                    -- _f pkg'
                    -- Alpm.dbGetPkg db n
                    return pkg'
                _ -> error "NOT IMPLEMENTED: Packages from local db"
            -- TODO: Do we need locking ?
            Alpm.addPkg h res
            Alpm.pkgGetName res
        addPkgs <- Alpm.transGetAdd h
        addPkgNames <- traverse Alpm.pkgGetName addPkgs
        logTrace_
            $ "Packages added by transaction: "
            <> display (show @Text addPkgNames)

        let
            allPkgNames = toList pkgNames <> addPkgNames
        installedPkgs <- Alpm.dbGetPkgcache localDb
        forM_ installedPkgs $ \pkg -> do
            n <- Alpm.pkgGetName pkg
            unless (n `elem` allPkgNames) $ do
                logInfo_ $ "Removing " <> display (show @Text n) <> " from transaction"
                Alpm.removePkg h pkg
        removePkgs <- Alpm.transGetRemove h

        if null $ addPkgs <> removePkgs
            then logInfo_ "Nothing to do"
            else do
                Alpm.transPrepare h
                Alpm.transCommit h
        pure pkgNames

    forM_ pkgNames $ \n -> do
        mpkg <- Alpm.dbGetPkg localDb n
        case mpkg of
            Nothing -> return () -- TODO
            Just !pkg' -> Alpm.pkgSetReason h pkg' AlpmPkgReasonExplicit

eventLogger :: (Log :> es) => AlpmEvent -> Eff es ()
eventLogger evt@(AlpmEvent AlpmEventCheckdepsStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventDatabaseMissing) = logAttention_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventDbRetrieveDone) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventDbRetrieveFailed) = logAttention_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventDbRetrieveStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventDiskspaceStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventFileconflictsStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventHookStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventHookRunStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventIntegrityStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventInterconflictsStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventKeyringStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventKeyDownloadStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventLoadStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventOptdepRemoval) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventPackageOperationStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventPacnewCreated) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventPacsaveCreated) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventPkgRetrieveFailed) = logAttention_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventPkgRetrieveStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventResolvedepsStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventScriptletInfo) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent AlpmEventTransactionStart) = logInfo_ $ display evt
eventLogger evt@(AlpmEvent _) = logTrace_ $ display evt
eventLogger evt = logInfo_ $ display evt

answering
    :: (IOE :> es, Log :> es)
    => Package
    -> AlpmQuestion
    -> Eff es ()
answering Package{name, providers} (AlpmQuestionSelectProvider' dep ps cb) = do
    dep' <- Alpm.depComputeString dep
    ps' <- mapM (fmap Text.pack . Alpm.pkgGetName) ps
    case fmap head . nonEmpty $ List.intersect providers ps' of
        Just provider | Just i <- List.elemIndex provider ps' -> do
            logInfo_
                $ "Target "
                <> display name
                <> ": Choosing provider "
                <> display provider
                <> " for "
                <> display (Text.pack dep')
            liftIO $ cb i
        _ -> throwM $ NoProviderFound name dep' providers ps'
answering pkg (AlpmQuestion qt) = logTrace_ $ display (show @Text (pkg, qt))

getSyncPkg
    :: (IOE :> es, Log :> es)
    => AlpmHandlePtr
    -> Package
    -> Eff es (Maybe AlpmPkgPtr)
getSyncPkg h pkg@(Package{..}) = do
    logTrace_ $ "Getting sync package: " <> display name
    let
        ns = Vector.map databaseNameToString databases
    dbs <-
        Alpm.getSyncdbs h
            >>= filterM
                ( \db -> do
                    n <- Alpm.dbGetName db
                    pure $ n `Vector.elem` ns
                )
    withEffToIO (ConcUnlift Ephemeral Unlimited) $ \runInIO ->
        Alpm.optionSetQuestionCb h (runInIO . answering pkg)
    res <- Alpm.findDbsSatisfier' h dbs $ mkDepend name versions
    logTrace_ $ "Got sync package: " <> display (show @Text name)
    return res
    where
        registerDatabase pkgsiglevels db =
            Alpm.registerSyncdb
                h
                (databaseNameToString db)
                (pkgsiglevels <> toAlpmDatabaseSiglevels sigcheck sigtrust)

getBuildPkg
    :: (FileSystem :> es, IOE :> es, Log :> es, TypedProcess :> es)
    => AlpmHandlePtr
    -> Package
    -> Build
    -> Eff es AlpmPkgPtr
getBuildPkg h (Package{..}) (Build{..}) = do
    logTrace_ $ "Getting build package: " <> display name
    path' <- resolveFile' path
    unless (Text.null script)
        $ unlessM (doesFileExist path')
        $ withSystemTempFile "dhalpm-"
        $ \scriptFp fh -> do
            Effectful.FileSystem.IO.ByteString.hPut fh $ encodeUtf8 script
            hClose fh
            runProcess_ $ proc "sh" [fromAbsFile scriptFp]
    Alpm.pkgLoad h True (toAlpmPackageSiglevels sigcheck sigtrust)
        $ fromAbsFile path'

getLocalPkg
    :: (IOE :> es, Log :> es)
    => AlpmHandlePtr
    -> Package
    -> Eff es (Maybe AlpmPkgPtr)
getLocalPkg h Package{..} = do
    logTrace_ $ "Getting local package: " <> display name
    localDb <- Alpm.getLocaldb h
    Alpm.findDbsSatisfier' h [localDb] $ mkDepend name versions

mkDepend :: AlpmPkgName -> Versions -> AlpmDepend
mkDepend n v =
    AlpmDepend
        { alpmDependName = n
        , alpmDependConstraint = case v of
            Any -> ConstraintAny
            Lt v' -> ConstraintLT $ toAlpmVersion v'
            Le v' -> ConstraintLE $ toAlpmVersion v'
            Eq v' -> ConstraintEQ $ toAlpmVersion v'
            Ge v' -> ConstraintGE $ toAlpmVersion v'
            Gt v' -> ConstraintGT $ toAlpmVersion v'
        }
    where
        toAlpmVersion Version{..} =
            AlpmVersion
                { alpmVersionEpoch = epoch
                , alpmVersionVer = Text.unpack version
                , alpmVersionRel = rel
                , alpmVersionSubRel = subrel
                }

collectDatabases
    :: Vector Package -> Validation [Database] (HashMap Text Database)
collectDatabases = sequenceA . unionsWith f . fmap g . mconcatMap databases
    where
        f v@(Success x) (Success y) | x == y = v
        f (Success x) y = f (Failure [x]) y
        f x (Success y) = f x (Failure [y])
        f x y = x <> y

        g db@(Database{name}) = HashMap.singleton name $ Success db

registerDatabaseGlobal
    :: (IOE :> es, Log :> es)
    => AlpmHandlePtr
    -> Database
    -> Eff es AlpmDbPtr
registerDatabaseGlobal h Database{..} = do
    logTrace_ $ "Registering database " <> display name
    db <-
        Alpm.registerSyncdb h (Text.unpack name)
            $ toAlpmDatabaseSiglevels sigcheck sigtrust
    logTrace_
        $ "Setting servers for "
        <> display name
        <> ": "
        <> display (show @Text servers)
    Alpm.dbSetServers h db $ Vector.toList $ Vector.map Text.unpack servers
    return db

toAlpmDatabaseSiglevels :: SiglevelCheck -> SiglevelTrust -> [AlpmSiglevel]
toAlpmDatabaseSiglevels check trust = check' <> trust'
    where
        check' = case check of
            CheckNever -> []
            CheckOptional -> [AlpmSigDatabaseOptional]
            CheckRequired -> [AlpmSigDatabase]

        trust' = case trust of
            TrustAll -> [AlpmSigDatabaseUnknownOk, AlpmSigDatabaseMarginalOk]
            TrustMarginal -> [AlpmSigDatabaseMarginalOk]
            TrustFull -> []

toAlpmPackageSiglevels :: SiglevelCheck -> SiglevelTrust -> [AlpmSiglevel]
toAlpmPackageSiglevels check trust = check' <> trust'
    where
        check' = case check of
            CheckNever -> []
            CheckOptional -> [AlpmSigPackageOptional]
            CheckRequired -> [AlpmSigPackage]

        trust' = case trust of
            TrustAll -> [AlpmSigPackageUnknownOk, AlpmSigPackageMarginalOk]
            TrustMarginal -> [AlpmSigPackageMarginalOk]
            TrustFull -> []

toAlpmVersion :: Version -> AlpmVersion
toAlpmVersion ver =
    AlpmVersion
        { alpmVersionEpoch = epoch ver
        , alpmVersionVer = Text.unpack $ version ver
        , alpmVersionRel = rel ver
        , alpmVersionSubRel = subrel ver
        }

choiceM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
choiceM = foldr (\k memo -> k >>= maybe memo (return . Just)) (return Nothing)

mconcatMap :: (Foldable t, Monoid (f b)) => (a -> f b) -> t a -> f b
mconcatMap f = foldl' (\memo x -> memo <> f x) mempty

unionsWith
    :: (Foldable t, Hashable k) => (v -> v -> v) -> t (HashMap k v) -> HashMap k v
unionsWith f = foldl' (HashMap.unionWith f) mempty
