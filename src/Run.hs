{-# LANGUAGE OverloadedLists #-}
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

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
import Types.Dhall (
    Build,
    Config,
    Database,
    Package,
    SiglevelCheck (..),
    SiglevelTrust (..),
    Version,
    Versions (..),
 )

import Archlinux.Alpm qualified as Alpm
import Types.Dhall qualified as Config

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
        substitutions =
            Dhall.Map.fromList
                <$> sequenceA
                    [ ("Build/Type",) <$> Dhall.expected (Dhall.auto @Build)
                    , ("Config/Type",) <$> Dhall.expected (Dhall.auto @Config)
                    , ("Database/Type",) <$> Dhall.expected (Dhall.auto @Database)
                    , ("Package/Type",) <$> Dhall.expected (Dhall.auto @Package)
                    , ("SiglevelCheck/Type",) <$> Dhall.expected (Dhall.auto @SiglevelCheck)
                    , ("SiglevelTrust/Type",) <$> Dhall.expected (Dhall.auto @SiglevelTrust)
                    , ("Versions/Type",) <$> Dhall.expected (Dhall.auto @Versions)
                    , ("Version/Type",) <$> Dhall.expected (Dhall.auto @Version)
                    , --
                      pure ("Database", Config.embedDefault (Dhall.inject @Database))
                    , pure ("Package", Config.embedDefault (Dhall.inject @Package))
                    , pure ("SiglevelCheck", Config.embedDefault (Dhall.inject @SiglevelCheck))
                    , pure ("SiglevelTrust", Config.embedDefault (Dhall.inject @SiglevelTrust))
                    , pure ("Versions", Config.embedDefault (Dhall.inject @Versions))
                    -- , pure ("Version"      , embedDefault (Dhall.inject @Version      ))
                    ]
    substitutions' <- case substitutions of
        Failure e -> throwM e
        Success subst' -> pure subst'
    let
        settings =
            set Dhall.substitutions substitutions'
                $ Dhall.defaultEvaluateSettings
    config <- liftIO (Dhall.inputFileWithSettings settings Dhall.auto fp)
    rootdir <- resolveDir' (Config.configRootDir config)
    dbdir <- resolveDir' (Config.configDatabaseDir config)
    ensureDir dbdir
    Alpm.withAlpm (fromAbsDir rootdir) (fromAbsDir dbdir) $ \h ->
        run' rootdir dbdir (Config.configPackages config) h

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
    case collectDatabases packages of
        Failure es -> throwM $ ConflictingDatabaseDefinitions es
        Success dbs -> do
            dbs' <- traverse (registerDatabaseGlobal h) dbs
            updateResult <- Alpm.dbUpdate h dbs' False
            logTrace_ $ case updateResult of
                DbUpdated -> "Databases were updated"
                DbUpdateSkipped -> "Update of databases was skipped"
                DbUpToDate -> "Databases were up to date"
    pkgNames <- Alpm.withTrans h [AlpmTransFlagAlldeps, AlpmTransFlagNeeded] $ do
        -- TODO: Set trans flags from config ?
        -- pkgNames <- forConcurrently packages $ \pkg@(Package {..}) -> do
        pkgNames <- for packages $ \package -> do
            pkg' <- do
                mpkg <-
                    choiceM
                        [ getSyncPkg h package
                        , for
                            (Config.packageBuild package)
                            (getBuildPkg h package)
                        , getLocalPkg h package
                        ]
                case mpkg of
                    Nothing -> throwM $ PackageNotFound package
                    Just pkg' -> pure pkg'
            origin <- Alpm.pkgGetOrigin pkg'
            logTrace_
                $ "Origin of "
                <> display (Config.packageName package)
                <> ": "
                <> display (show @Text origin)
            res <- case origin of
                AlpmPkgFromFile -> do
                    file <- Alpm.pkgGetFilename pkg'
                    logInfo_ $ "Adding " <> display (show @Text file) <> " to transaction"
                    Alpm.pkgLoad
                        h
                        True
                        ( toAlpmPackageSiglevels
                            (Config.packageSigcheck package)
                            (Config.packageSigtrust package)
                        )
                        file
                AlpmPkgFromSyncdb ->
                    pure pkg'
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
            Nothing -> pure () -- TODO
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
answering package (AlpmQuestionSelectProvider' dependency candidates cb) = do
    let
        name = Config.packageName package
        providers = Config.packageProviders package

    dependency' <- Alpm.depComputeString dependency
    candidates' <- traverse (fmap Text.pack . Alpm.pkgGetName) candidates
    case fmap head . nonEmpty $ List.intersect (Vector.toList providers) candidates' of
        Just provider | Just i <- List.elemIndex provider candidates' -> do
            logInfo_
                $ "Target "
                <> display name
                <> ": Choosing provider "
                <> display provider
                <> " for "
                <> display (Text.pack dependency')
            liftIO $ cb i
        _ -> throwM $ NoProviderFound name dependency' providers candidates'
answering pkg (AlpmQuestion qt) = logTrace_ $ display (show @Text (pkg, qt))

getSyncPkg
    :: (IOE :> es, Log :> es)
    => AlpmHandlePtr
    -> Package
    -> Eff es (Maybe AlpmPkgPtr)
getSyncPkg h package = do
    let
        name = Config.packageName package
        versions = Config.packageVersions package
        dbNames =
            Vector.foldMap
                (Set.singleton . Config.databaseName)
                (Config.packageDatabases package)
    logTrace_ $ "Getting sync package: " <> display name
    dbs <-
        Alpm.getSyncdbs h
            >>= filterM
                ( \db -> do
                    n <- Alpm.dbGetName db
                    pure (Text.pack n `Set.member` dbNames)
                )
    withEffToIO (ConcUnlift Ephemeral Unlimited) $ \runInIO ->
        Alpm.optionSetQuestionCb h (runInIO . answering package)
    res <- Alpm.findDbsSatisfier' h dbs (mkDepend name versions)
    logTrace_ $ "Got sync package: " <> display (show @Text name)
    pure res

getBuildPkg
    :: (FileSystem :> es, IOE :> es, Log :> es, TypedProcess :> es)
    => AlpmHandlePtr
    -> Package
    -> Build
    -> Eff es AlpmPkgPtr
getBuildPkg h package build = do
    let
        name = Config.packageName package
        path = Config.buildPath build
        script = Config.buildScript build

    logTrace_ $ "Getting build package: " <> display name
    path' <- resolveFile' path
    unless (Text.null script)
        $ unlessM (doesFileExist path')
        $ withSystemTempFile "dhalpm-"
        $ \scriptFp fh -> do
            Effectful.FileSystem.IO.ByteString.hPut fh $ encodeUtf8 script
            hClose fh
            runProcess_ $ proc "sh" [fromAbsFile scriptFp]
    Alpm.pkgLoad
        h
        True
        ( toAlpmPackageSiglevels
            (Config.packageSigcheck package)
            (Config.packageSigtrust package)
        )
        (fromAbsFile path')

getLocalPkg
    :: (IOE :> es, Log :> es)
    => AlpmHandlePtr
    -> Package
    -> Eff es (Maybe AlpmPkgPtr)
getLocalPkg h package = do
    let
        name = Config.packageName package
        versions = Config.packageVersions package
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

collectDatabases
    :: Vector Package
    -> Validation (NonEmpty Database) [Database]
collectDatabases =
    fmap Map.elems
        . sequenceA
        . Map.unionsWith validate
        . fmap pack
        . foldMap Config.packageDatabases
    where
        validate
            :: Validation (NonEmpty Database) Database
            -> Validation (NonEmpty Database) Database
            -> Validation (NonEmpty Database) Database
        validate v@(Success x) (Success y)
            | x == y = v
            | otherwise = Failure [x, y]
        validate (Success x) y = validate (Failure [x]) y
        validate x (Success y) = validate x (Failure [y])
        validate x y = x <> y

        pack :: Database -> Map Text (Validation (NonEmpty Database) Database)
        pack db = Map.singleton (Config.databaseName db) (Success db)

registerDatabaseGlobal
    :: (IOE :> es, Log :> es)
    => AlpmHandlePtr
    -> Database
    -> Eff es AlpmDbPtr
registerDatabaseGlobal h db = do
    let
        name = Config.databaseName db
        servers = Config.databaseServers db
    logTrace_ $ "Registering database " <> display name
    dbPtr <-
        Alpm.registerSyncdb h (Text.unpack name)
            $ toAlpmDatabaseSiglevels
                (Config.databaseSigcheck db)
                (Config.databaseSigtrust db)
    logTrace_
        $ "Setting servers for "
        <> display name
        <> ": "
        <> display (show @Text servers)
    Alpm.dbSetServers h dbPtr
        . Vector.toList
        . Vector.map Text.unpack
        $ servers
    pure dbPtr

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
toAlpmVersion version =
    AlpmVersion
        { alpmVersionEpoch = Config.versionEpoch version
        , alpmVersionVer = Text.unpack (Config.versionVersion version)
        , alpmVersionRel = Config.versionRel version
        , alpmVersionSubRel = Config.versionSubrel version
        }

choiceM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
choiceM = foldr (\k memo -> k >>= maybe memo (pure . Just)) (pure Nothing)
