{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Run (run) where

import Import

import Data.Either.Validation
import Dhall (auto, defaultEvaluateSettings, expected, inject, inputFileWithSettings, substitutions)
import qualified Dhall.Map
import qualified RIO.ByteString as BS
import qualified RIO.HashMap as HM
import qualified RIO.List as L
import RIO.Process (proc, runProcess_)
import qualified RIO.Text as T
import qualified RIO.Vector as V

import Archlinux.Alpm

default (Text)



instance Eq Version where
    x == y = toAlpmVersion x == toAlpmVersion y

instance Ord Version where
    x `compare` y = toAlpmVersion x `compare` toAlpmVersion y



run :: RIO App ()
run = do
    fp <- fromMaybe "config.dhall" <$> asks (optionsConfig . appOptions)
    let subst = Dhall.Map.fromList <$> sequenceA
            [ ("BuildT"        ,) <$> expected (auto @Build        )
            , ("ConfigT"       ,) <$> expected (auto @Config       )
            , ("DatabaseT"     ,) <$> expected (auto @Database     )
            , ("PackageT"      ,) <$> expected (auto @Package      )
            , ("SiglevelCheckT",) <$> expected (auto @SiglevelCheck)
            , ("SiglevelTrustT",) <$> expected (auto @SiglevelTrust)
            , ("VersionsT"     ,) <$> expected (auto @Versions     )
            , ("VersionT"      ,) <$> expected (auto @Version      )
            --
            , pure ("Database"     , embedDefault (inject @Database     ))
            , pure ("Package"      , embedDefault (inject @Package      ))
            , pure ("SiglevelCheck", embedDefault (inject @SiglevelCheck))
            , pure ("SiglevelTrust", embedDefault (inject @SiglevelTrust))
            , pure ("Versions"     , embedDefault (inject @Versions     ))
            --, pure ("Version"      , embedDefault (inject @Version      ))
            ]
    subst' <- case subst of
        Failure e -> throwM e
        Success subst' -> pure subst'
    let opts = set substitutions subst' defaultEvaluateSettings
    config <- liftIO $ inputFileWithSettings opts  auto fp
    rootdir <- resolveDir' $ rootDir config
    dbdir <- resolveDir' $ databaseDir config
    ensureDir dbdir
    withAlpm (fromAbsDir rootdir) (fromAbsDir dbdir) $ \h ->
        run' rootdir dbdir (packages config) h

run' :: Path Abs Dir -> Path Abs Dir -> Vector Package -> AlpmHandlePtr -> RIO App ()
run' rootdir dbdir packages h = do
    optionSetEventCb h eventLogger
    localDb <- getLocaldb h
    dbs <- case collectDatabases packages of
        Failure es -> throwM $ ConflictingDatabaseDefinitions es
        Success dbs -> do
            dbs' <- mapM (registerDatabaseGlobal h) dbs
            updateResult <- dbUpdate h (HM.elems dbs') False
            logDebug $ case updateResult of
                DbUpdated -> "Databases were updated"
                DbUpdateSkipped -> "Update of databases was skipped"
                DbUpToDate -> "Databases were up to date"
            pure dbs'
    withTrans h [AlpmTransFlagAlldeps, AlpmTransFlagNeeded] $ do  -- TODO: Set trans flags from config ?
        --pkgNames <- forConcurrently packages $ \pkg@(Package {..}) -> do
        pkgNames <- flip mapM packages $ \pkg@(Package {..}) -> do
            pkg' <- do
                mpkg <- choiceM
                    [ getSyncPkg h pkg
                    , for build (getBuildPkg h pkg)
                    , getLocalPkg h pkg
                    ]
                case mpkg of
                    Nothing -> throwM $ PackageNotFound pkg
                    Just pkg' -> return pkg'
            orig <- pkgGetOrigin pkg'
            logDebug $ "Origin of " <> display name <> ": " <> displayShow orig
            res <- case orig of
                AlpmPkgFromFile -> do
                    fn <- pkgGetFilename pkg'
                    logInfo $ "Adding " <> displayShow fn <> " to transaction"
                    pkgLoad h True (toAlpmPackageSiglevels sigcheck sigtrust) fn
                AlpmPkgFromSyncdb -> do
                    db <- pkgGetDb pkg' >>= \mdb -> case mdb of
                        Nothing -> error $ "SHOULD NEVER HAPPEN: No database for " <> packageNameToString pkg
                        Just db -> do
                            dbname <- dbGetName db
                            case HM.lookup (T.pack dbname) dbs of
                                Nothing -> error $ "SHOULD NEVER HAPPEN: Database " <> dbname <> " not found"
                                Just db' -> return db'
                    n <- pkgGetName pkg'
                    logInfo $ "Adding " <> displayShow n <> " to transaction"
                    dbGetPkg db n
                _ -> error "NOT IMPLEMENTED: Packages from local db"
            -- TODO: Locking ?
            addPkg h res
            pkgGetName res
        addPkgs <- transGetAdd h
        addPkgNames <- mapM pkgGetName addPkgs
        logDebug $ "Packages added by transaction: " <> displayShow addPkgNames

        installedPkgs <- dbGetPkgcache localDb
        forM_ installedPkgs $ \pkg -> do
            n <- pkgGetName pkg
            unless (n `elem` addPkgNames) $ do
                logInfo $ "Removing " <> displayShow n <> " to transaction"
                removePkg h pkg
        removePkgs <- transGetRemove h

        if null $ addPkgs <> removePkgs
            then logInfo "Nothing to do"
            else do
                transPrepare h
                transCommit h

        forM_ pkgNames $ \n -> do
            pkg <- dbGetPkg localDb n
            pkgSetReason h pkg AlpmPkgReasonExplicit

eventLogger :: AlpmEvent -> RIO App ()
eventLogger evt@(AlpmEvent AlpmEventCheckdepsStart)        = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventDatabaseMissing)       = logWarn  $ display evt
eventLogger evt@(AlpmEvent AlpmEventDbRetrieveDone)        = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventDbRetrieveFailed)      = logError $ display evt
eventLogger evt@(AlpmEvent AlpmEventDbRetrieveStart)       = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventDiskspaceStart)        = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventFileconflictsStart)    = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventHookStart)             = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventHookRunStart)          = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventIntegrityStart)        = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventInterconflictsStart)   = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventKeyringStart)          = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventKeyDownloadStart)      = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventLoadStart)             = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventOptdepRemoval)         = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventPackageOperationStart) = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventPacnewCreated)         = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventPacsaveCreated)        = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventPkgRetrieveFailed)     = logError $ display evt
eventLogger evt@(AlpmEvent AlpmEventPkgRetrieveStart)      = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventResolvedepsStart)      = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventScriptletInfo)         = logInfo  $ display evt
eventLogger evt@(AlpmEvent AlpmEventTransactionStart)      = logInfo  $ display evt
eventLogger evt@(AlpmEvent _) = logDebug $ display evt
eventLogger evt = logInfo $ display evt

answering :: Package -> AlpmQuestion -> RIO App ()
answering Package {name, providers} (AlpmQuestionSelectProvider' dep ps cb) = do
    dep' <- depComputeString dep
    ps' <- mapM (fmap T.pack . pkgGetName) ps
    case L.headMaybe $ L.intersect providers ps' of
        Just provider | Just i <- L.elemIndex provider ps' -> do
            logInfo $ "Target " <> display name <> ": Choosing provider " <> display provider <> " for " <> display (T.pack dep')
            liftIO $ cb i
        _ -> throwM $ NoProviderFound name dep' providers ps'
answering pkg (AlpmQuestion qt) = logDebug $ displayShow (pkg, qt)

getSyncPkg :: AlpmHandlePtr -> Package -> RIO App (Maybe AlpmPkgPtr)
getSyncPkg h pkg@(Package {..}) = do
    logDebug $ "Getting sync package: " <> display name
    let ns = V.map databaseNameToString databases
    dbs <- getSyncdbs h >>= filterM (\db -> do
        n <- dbGetName db
        pure $ n `V.elem` ns
        )
    optionSetQuestionCb h $ answering pkg
    res <- findDbsSatisfier' h dbs $ mkDepend name versions
    logDebug $ "Got sync package: " <> displayShow name
    return res
    where
        registerDatabase pkgsiglevels db = registerSyncdb h
            (databaseNameToString db)
            (pkgsiglevels <> toAlpmDatabaseSiglevels sigcheck sigtrust)

getBuildPkg :: AlpmHandlePtr -> Package -> Build -> RIO App AlpmPkgPtr
getBuildPkg h (Package {..}) (Build {..}) = do
    logDebug $ "Getting build package: " <> display name
    path' <- resolveFile' path
    unless (T.null script) $ unlessM (doesFileExist path') $
        withSystemTempFile "dhalpm-" $ \scriptFp fh -> do
            BS.hPut fh $ encodeUtf8 script
            hClose fh
            proc "sh" [fromAbsFile scriptFp] runProcess_
    pkgLoad h True (toAlpmPackageSiglevels sigcheck sigtrust) $ fromAbsFile path'

getLocalPkg :: AlpmHandlePtr -> Package -> RIO App (Maybe AlpmPkgPtr)
getLocalPkg h Package {..} = do
    logDebug $ "Getting local package: " <> display name
    localDb <- getLocaldb h
    findDbsSatisfier' h [localDb] $ mkDepend name versions

mkDepend :: AlpmPkgName -> Versions -> AlpmDepend
mkDepend n v = AlpmDepend
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
        toAlpmVersion Version{..} = AlpmVersion
            { alpmVersionEpoch = epoch
            , alpmVersionVer = T.unpack version
            , alpmVersionRel = rel
            , alpmVersionSubRel = subrel
            }

collectDatabases :: Vector Package -> Validation [Database] (HashMap Text Database)
collectDatabases = traverse id . unionsWith f . fmap g . mconcatMap databases
    where
        f v@(Success x) (Success y) | x == y = v
        f (Success x) y = f (Failure [x]) y
        f x (Success y) = f x (Failure [y])
        f x y = x <> y

        g db@(Database {name}) = HM.singleton name $ Success db

registerDatabaseGlobal :: AlpmHandlePtr -> Database -> RIO App AlpmDbPtr
registerDatabaseGlobal h Database {..} = do
    logDebug $ "Registering database " <> display name
    db <- registerSyncdb h (T.unpack name) $ toAlpmDatabaseSiglevels sigcheck sigtrust
    logDebug $ "Setting servers for " <> display name <> ": " <> displayShow servers
    dbSetServers h db $ V.toList $ V.map T.unpack servers
    return db

toAlpmDatabaseSiglevels :: SiglevelCheck -> SiglevelTrust -> [AlpmSiglevel]
toAlpmDatabaseSiglevels check trust = check' <> trust'
    where
        check' = case check of
            CheckNever    -> []
            CheckOptional -> [AlpmSigDatabaseOptional]
            CheckRequired -> [AlpmSigDatabase]

        trust' = case trust of
            TrustAll      -> [AlpmSigDatabaseUnknownOk, AlpmSigDatabaseMarginalOk]
            TrustMarginal -> [AlpmSigDatabaseMarginalOk]
            TrustFull     -> []

toAlpmPackageSiglevels :: SiglevelCheck -> SiglevelTrust -> [AlpmSiglevel]
toAlpmPackageSiglevels check trust = check' <> trust'
    where
        check' = case check of
            CheckNever    -> []
            CheckOptional -> [AlpmSigPackageOptional]
            CheckRequired -> [AlpmSigPackage]

        trust' = case trust of
            TrustAll      -> [AlpmSigPackageUnknownOk, AlpmSigPackageMarginalOk]
            TrustMarginal -> [AlpmSigPackageMarginalOk]
            TrustFull     -> []

toAlpmVersion :: Version -> AlpmVersion
toAlpmVersion ver = AlpmVersion
    { alpmVersionEpoch  = epoch ver
    , alpmVersionVer    = T.unpack $ version ver
    , alpmVersionRel    = rel ver
    , alpmVersionSubRel = subrel ver
    }

choiceM :: Monad m => [m (Maybe a)] -> m (Maybe a)
choiceM = foldr (\k memo -> k >>= maybe memo (return . Just)) (return Nothing)

mconcatMap :: (Foldable t, Monoid (f b)) => (a -> f b) -> t a -> f b
mconcatMap f = foldl' (\memo x -> memo <> f x) mempty

unionsWith :: (Foldable t, Hashable k) => (v -> v -> v) -> t (HashMap k v) -> HashMap k v
unionsWith f = foldl' (HM.unionWith f) mempty
