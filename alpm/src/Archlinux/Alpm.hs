module Archlinux.Alpm (
    AlpmConstraint (..),
    AlpmDepend (..),
    AlpmDependPtr,
    AlpmDepmissing (..),
    AlpmDepmod (..),
    AlpmGroup (..),
    AlpmError (..),
    AlpmUnknownError (..),
    AlpmDbError (..),
    AlpmPkgError (..),
    AlpmTransactionError (..),
    AlpmErrNo (..),
    AlpmEvent (..),
    AlpmEventType (..),
    AlpmEventFor (..),
    AlpmHandlePtr,
    AlpmDbPtr,
    AlpmPkg (..),
    AlpmPkgfrom (..),
    AlpmPkgName (..),
    AlpmPkgPtr,
    AlpmPkgreason (..),
    AlpmQuestion (..),
    AlpmQuestionType (..),
    AlpmQuestionFor (..),
    AlpmSiglevel (..),
    AlpmTransflag (..),
    AlpmVersion (..),
    UpdateResult (..),
    AlpmPkgNameParseException (..),
    AlpmVersionParseException (..),
    --
    emptyAlpmPkgName,
    errno,
    strerror,
    withAlpm,
    initialize,
    release,
    getLocaldb,
    getSyncdbs,
    registerSyncdb,
    pkgLoad,
    optionSetEventCb,
    optionSetQuestionCb,
    dbSetUsage,
    dbGetServers,
    dbAddServer,
    dbSetServers,
    dbRemoveServer,
    dbGetGroup,
    dbGetGroupcache,
    dbGetPkg,
    dbGetPkgcache,
    dbSearch,
    dbUpdate,
    findDbsSatisfier,
    findDbsSatisfier',
    pkgFree,
    pkgGetDb,
    pkgGetDepends,
    pkgGetFilename,
    pkgGetName,
    pkgGetOrigin,
    pkgGetProvides,
    pkgGetVersion,
    pkgSetReason,
    withTrans,
    transPrepare,
    transCommit,
    transInterrupt,
    transRelease,
    transGetAdd,
    transGetRemove,
    syncSysupgrade,
    addPkg,
    removePkg,
    depComputeString,
    depFromString,
    depFree,
    --
    fromAlpmList,
    toAlpmList,
    withAlpmList,
    parsePackageNameFromText,
    parseVersionFromText,
    -- , pkgNameP
    -- , alpmVersionP
    showAlpmVersion,
    -- Testing only
    dbGetName,
    withCStrings,
    --
    module Archlinux.Alpm.Types,
) where

import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C
import UnliftIO (MonadUnliftIO, askUnliftIO, unliftIO)
import UnliftIO.Exception (bracket)
import Prelude

import Data.ByteString.Short.Internal qualified as ShortByteString
import Data.Text.Short.Unsafe qualified as ShortText

import Archlinux.Alpm.Binding hiding (initialize, release, errno, strerror)
import Archlinux.Alpm.Binding qualified as Binding
import Archlinux.Alpm.Package.Types hiding (name, version)
import Archlinux.Alpm.Types
import Archlinux.Alpm.Utils

import ShortText.Extra qualified as ShortText

--------------------------------------------------------------------------------
-- Handles
--------------------------------------------------------------------------------

withAlpm
    :: (MonadUnliftIO m) =>
    FilePath -> FilePath -> (AlpmHandlePtr -> m a) -> m a
withAlpm rootDir dbDir = bracket (initialize rootDir dbDir) release

initialize :: (MonadIO m) => FilePath -> FilePath -> m AlpmHandlePtr
initialize rootDir dbDir = liftIO $ do
    rootDir' <- newCString rootDir
    dbDir' <- newCString dbDir
    bracket calloc free $ \e -> do
        h@(AlpmHandlePtr p) <- Binding.initialize rootDir' dbDir' e
        if p == nullPtr
            then do
                e' <- peek e
                throwM $ AlpmError (toEnum (fromIntegral e')) InitializationError
            else pure h

release :: (MonadIO m) => AlpmHandlePtr -> m Int
release = fmap fromIntegral . liftIO . Binding.release

syncSysupgrade :: (MonadIO m) => AlpmHandlePtr -> Bool -> m ()
syncSysupgrade h = liftIO .
    throwAlpmError h . Binding.sync_sysupgrade h . fromInteger . fromBool

optionSetEventCb
    :: (MonadUnliftIO m) =>
        AlpmHandlePtr -> (AlpmEvent -> m ()) -> m ()
optionSetEventCb h cb = do
    u <- askUnliftIO
    liftIO $ do
        cb' <- createAlpmCbEventFunPtr $ \_ p -> unliftIO u $ do
            evt <- liftIO $ peekAlpmEvent $ castPtr p
            cb evt
        throwAlpmError' h SetEventCbError $
            Binding.option_set_eventcb h cb' nullPtr

foreign import ccall "wrapper"
    createAlpmCbEventFunPtr :: (Ptr () -> Ptr () -> IO ()) -> IO AlpmCbEventFunPtr

optionSetQuestionCb
    :: (MonadUnliftIO m) =>
        AlpmHandlePtr -> (AlpmQuestion -> m ()) -> m ()
optionSetQuestionCb h cb = do
    u <- askUnliftIO
    liftIO $ do
        cb' <- createAlpmCbQuestionFunPtr $ \_ p -> unliftIO u $ do
            q <- liftIO $ peekAlpmQuestion (castPtr p)
            cb q
        throwAlpmError' h SetQuestionCbError $
            Binding.option_set_questioncb h cb' nullPtr

foreign import ccall "wrapper"
    createAlpmCbQuestionFunPtr
        :: (Ptr () -> Ptr () -> IO ()) -> IO AlpmCbQuestionFunPtr

data AlpmHandleError
    = InitializationError
    | SetEventCbError
    | SetQuestionCbError
    deriving (Eq, Show)

instance Exception AlpmHandleError

--------------------------------------------------------------------------------
-- Databases
--------------------------------------------------------------------------------

getLocaldb :: (MonadIO m) => AlpmHandlePtr -> m AlpmDbPtr
getLocaldb = liftIO . Binding.get_localdb

getSyncdbs :: (MonadIO m) => AlpmHandlePtr -> m [AlpmDbPtr]
getSyncdbs h = liftIO $ fromAlpmList =<< Binding.get_syncdbs h

registerSyncdb
    :: (MonadIO m) => AlpmHandlePtr -> String -> [AlpmSiglevel] -> m AlpmDbPtr
registerSyncdb h xs siglevel = liftIO $ do
    xs' <- newCString xs
    Binding.register_syncdb h xs' (makeBitmask siglevel)

dbGetName :: (MonadIO m) => AlpmDbPtr -> m String
dbGetName db = liftIO $ peekCString =<< Binding.db_get_name db

dbSetUsage :: (MonadIO m) => AlpmHandlePtr -> AlpmDbPtr -> [AlpmDbUsage] -> m ()
dbSetUsage h db xs = liftIO $ do
        throwAlpmErrorM' h (DbSetUsageError xs <$> dbGetName db) $
            Binding.db_set_usage db (makeBitmask xs)

dbGetServers :: (MonadIO m) => AlpmDbPtr -> m [String]
dbGetServers db = liftIO $ peekAlpmStringList =<< Binding.db_get_servers db

dbAddServer :: (MonadIO m) => AlpmHandlePtr -> AlpmDbPtr -> String -> m ()
dbAddServer h db xs = liftIO $ do
    xs' <- newCString xs
    throwAlpmErrorM' h (DbAddServerError xs <$> dbGetName db) $
        Binding.db_add_server db xs'

dbSetServers :: (MonadIO m) => AlpmHandlePtr -> AlpmDbPtr -> [String] -> m ()
dbSetServers h db xss = liftIO $ do
    xss' <- mapM newCString xss
    xs <- toAlpmList xss'
    throwAlpmErrorM' h (DbSetServersError xss <$> dbGetName db) $
        Binding.db_set_servers db xs

dbRemoveServer :: (MonadIO m) => AlpmHandlePtr -> AlpmDbPtr -> String -> m ()
dbRemoveServer h db xs = liftIO $ do
    xs' <- newCString xs
    throwAlpmErrorM' h (DbRemoveServerError xs <$> dbGetName db) $
        Binding.db_remove_server db xs'

dbGetGroup :: (MonadIO m) => AlpmDbPtr -> String -> m AlpmGroup
dbGetGroup db xs = liftIO $ do
    xs' <- newCString xs
    peekAlpmGroup =<< Binding.db_get_group db xs'

dbGetGroupcache :: (MonadIO m) => AlpmDbPtr -> m [AlpmGroup]
dbGetGroupcache db = liftIO $ do
    peekAlpmList peekAlpmGroup =<< Binding.db_get_groupcache db

dbGetPkg :: (MonadIO m) => AlpmDbPtr -> AlpmPkgName -> m (Maybe AlpmPkgPtr)
dbGetPkg h name = liftIO $ do
    xs <- ShortText.newCString (unAlpmPkgName name)
    ptr <- Binding.db_get_pkg h xs
    pure $
        if ptr == nullPtr
            then Nothing
            else Just ptr

dbGetPkgcache :: (MonadIO m) => AlpmDbPtr -> m [AlpmPkgPtr]
dbGetPkgcache db = liftIO $ fromAlpmList =<< Binding.db_get_pkgcache db

dbSearch
    :: (MonadIO m) =>
        AlpmHandlePtr -> AlpmDbPtr -> [String] -> m [AlpmPkgPtr]
dbSearch h db rxs = liftIO $ do
    withCStrings rxs $ \rxs' ->
        alloca $ \ret -> do
            needles <- toAlpmList rxs'
            throwAlpmErrorM' h (DbSearchError <$> dbGetName db <*> return rxs) $
                Binding.db_search db needles ret
            fromAlpmList =<< peek ret

dbUpdate :: (MonadIO m) =>
    AlpmHandlePtr -> [AlpmDbPtr] -> Bool -> m UpdateResult
dbUpdate _ [] _ = pure DbUpdateSkipped
dbUpdate h dbs doForce = liftIO $ do
    dbs' <- toAlpmList dbs
    ec <- Binding.db_update h dbs' (fromBool doForce)
    case ec of
        0 -> pure DbUpdated
        1 -> pure DbUpToDate
        _ -> do
            err <- errno h
            ns <- traverse dbGetName dbs
            throwM (AlpmError err (DbUpdateError ns))

findDbsSatisfier
    :: (MonadIO m) =>
    AlpmHandlePtr -> [AlpmDbPtr] -> String -> m (Maybe AlpmPkgPtr)
findDbsSatisfier _ [] _ = pure Nothing
findDbsSatisfier h dbs xs = liftIO $ do
    withAlpmList dbs $ \dbs' ->
        withCString xs $ \xs' -> do
            p <- Binding.find_dbs_satisfier h dbs' xs'
            pure $
                if p == nullPtr
                    then Nothing
                    else Just p

findDbsSatisfier'
    :: (MonadIO m)
    => AlpmHandlePtr -> [AlpmDbPtr] -> AlpmDepend -> m (Maybe AlpmPkgPtr)
findDbsSatisfier' _ [] _ = pure Nothing
findDbsSatisfier' h dbs x = liftIO . alloca $ \p -> do
    poke p x
    xs <- depComputeString p
    -- depFree p
    findDbsSatisfier h dbs xs

data UpdateResult
    = DbUpdated
    | DbUpdateSkipped
    | DbUpToDate
    deriving (Eq, Show)

data AlpmDbError
    = DbAddServerError String String
    | DbSetServersError [String] String
    | DbSetUsageError [AlpmDbUsage] String
    | DbRemoveServerError String String
    | DbSearchError String [String]
    | DbUpdateError [String]
    deriving (Eq, Show)

instance Exception AlpmDbError

--------------------------------------------------------------------------------
-- Packages
--------------------------------------------------------------------------------

pkgLoad
    :: (MonadIO m)
    => AlpmHandlePtr -> Bool -> [AlpmSiglevel] -> FilePath -> m AlpmPkgPtr
pkgLoad h full siglevel fp = liftIO $ do
    fp' <- newCString fp
    res <- malloc
    throwAlpmError' h (PkgLoadError fp) $
        Binding.pkg_load h fp' (fromInteger (fromBool full)) (makeBitmask siglevel) res
    peek res

pkgFree :: (MonadIO m) => AlpmHandlePtr -> AlpmPkgPtr -> m ()
pkgFree h pkg = liftIO $ do
        throwAlpmErrorM' h (PkgFreeError <$> pkgGetName pkg) $
            Binding.pkg_free pkg

pkgGetFilename :: (MonadIO m) => AlpmPkgPtr -> m FilePath
pkgGetFilename pkg = liftIO $ peekCString =<< Binding.pkg_get_filename pkg

pkgGetOrigin :: (MonadIO m) => AlpmPkgPtr -> m AlpmPkgfrom
pkgGetOrigin = liftIO . fmap (toEnum . fromIntegral) . Binding.pkg_get_origin

pkgGetDb :: (MonadIO m) => AlpmPkgPtr -> m (Maybe AlpmDbPtr)
pkgGetDb pkg = liftIO $ do
    p <- Binding.pkg_get_db pkg
    pure $
        if p == nullPtr
            then Nothing
            else Just p

pkgGetDepends :: (MonadIO m) => AlpmPkgPtr -> m [AlpmDepend]
pkgGetDepends pkg = liftIO $ do
    res <- Binding.pkg_get_depends pkg
    peekAlpmList peek res

pkgGetName :: (MonadIO m) => AlpmPkgPtr -> m AlpmPkgName
pkgGetName pkg = liftIO $ do
    res <- Binding.pkg_get_name pkg
    AlpmPkgName . ShortText.fromShortByteStringUnsafe
        <$> ShortByteString.packCString res

pkgGetProvides :: (MonadIO m) => AlpmPkgPtr -> m [AlpmDepend]
pkgGetProvides pkg = liftIO $ do
    res <- Binding.pkg_get_provides pkg
    peekAlpmList peek res

pkgGetVersion :: (MonadIO m) => AlpmPkgPtr -> m String
pkgGetVersion pkg = liftIO $ do
    res <- Binding.pkg_get_version pkg
    peekCString res

pkgSetReason
    :: (MonadIO m) => AlpmHandlePtr -> AlpmPkgPtr -> AlpmPkgreason -> m ()
pkgSetReason h pkg x = liftIO $ do
        throwAlpmErrorM' h (PkgSetReasonError <$> pkgGetName pkg <*> pure x) $
            (Binding.pkg_set_reason pkg $ fromIntegral $ fromEnum x)

data AlpmPkgError
    = PkgFreeError AlpmPkgName
    | PkgLoadError FilePath
    | PkgSetReasonError AlpmPkgName AlpmPkgreason
    deriving (Eq, Show)

instance Exception AlpmPkgError

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

depComputeString :: (MonadIO m) => AlpmDependPtr -> m String
depComputeString dep = liftIO $ Binding.dep_compute_string dep >>= peekCString

depFromString :: (MonadIO m) => String -> m AlpmDependPtr
depFromString xs = liftIO $ withCString xs Binding.dep_from_string

depFree :: (MonadIO m) => AlpmDependPtr -> m ()
depFree = liftIO . Binding.dep_free

--------------------------------------------------------------------------------
-- Groups
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

withTrans :: (MonadUnliftIO m) => AlpmHandlePtr -> [AlpmTransflag] -> m a -> m a
withTrans h flags f = bracket (transInit h flags) (\_ -> transRelease h) (\_ -> f)

transInit :: (MonadIO m) => AlpmHandlePtr -> [AlpmTransflag] -> m ()
transInit h flags = liftIO $ do
        throwAlpmError h (Binding.trans_init h (makeBitmask flags))

transPrepare :: (MonadIO m) => AlpmHandlePtr -> m ()
transPrepare h = liftIO $
    alloca $ \p -> do
        let
            onErr :: AlpmErrNo -> AlpmUnknownError -> IO AlpmTransactionError
            onErr err _ = do
                p' <- peek p
                case err of
                    AlpmErrPkgInvalidArch ->
                        TransPrepareTextError
                            <$> peekAlpmStringListFree p'
                    AlpmErrUnsatisfiedDeps ->
                        TransPrepareDepmissingError
                            <$> peekAlpmListFree peekAlpmDepmissing (Binding.depmissing_free . castPtr) p'
                    AlpmErrConflictingDeps ->
                        TransPrepareConflictError
                            <$> peekAlpmListFree peekAlpmConflict (Binding.conflict_free . castPtr) p'
                    _ -> return TransPrepareOtherError
        modifyAlpmErrorM onErr $ throwAlpmError h (Binding.trans_prepare h p)
        p' <- peek p
        Binding.list_free p'

transCommit :: (MonadIO m) => AlpmHandlePtr -> m ()
transCommit h = liftIO $
    alloca $ \p -> do
        let
            onErr :: AlpmErrNo -> AlpmUnknownError -> IO AlpmTransactionError
            onErr err _ = do
                p' <- peek p
                case err of
                    AlpmErrFileConflicts ->
                        TransCommitFileconflictError
                            <$> peekAlpmListFree peekAlpmFileconflict (Binding.fileconflict_free . castPtr) p'
                    AlpmErrPkgInvalid ->
                        TransCommitTextError
                            <$> peekAlpmStringListFree p'
                    AlpmErrPkgInvalidChecksum ->
                        TransCommitTextError
                            <$> peekAlpmStringListFree p'
                    AlpmErrPkgInvalidSig ->
                        TransCommitTextError
                            <$> peekAlpmStringListFree p'
                    _ -> return TransCommitOtherError
        modifyAlpmErrorM onErr $ throwAlpmError h (Binding.trans_commit h p)

transInterrupt :: (MonadIO m) => AlpmHandlePtr -> m ()
transInterrupt h = liftIO $ throwAlpmError h (Binding.trans_interrupt h)

transRelease :: (MonadIO m) => AlpmHandlePtr -> m ()
transRelease h = liftIO $ throwAlpmError h (Binding.trans_release h)

transGetAdd :: (MonadIO m) => AlpmHandlePtr -> m [AlpmPkgPtr]
transGetAdd h = liftIO $ fromAlpmList =<< Binding.trans_get_add h

transGetRemove :: (MonadIO m) => AlpmHandlePtr -> m [AlpmPkgPtr]
transGetRemove h = liftIO $ fromAlpmList =<< Binding.trans_get_remove h

addPkg :: (MonadIO m) => AlpmHandlePtr -> AlpmPkgPtr -> m ()
addPkg h pkg = liftIO $ do
        throwAlpmErrorM' h (AddPkgError <$> pkgGetName pkg) $
            Binding.add_pkg h pkg

removePkg :: (MonadIO m) => AlpmHandlePtr -> AlpmPkgPtr -> m ()
removePkg h pkg = liftIO $ do
        throwAlpmErrorM' h (RemovePkgError <$> pkgGetName pkg) $
            Binding.remove_pkg h pkg

data AlpmTransactionError
    = AddPkgError AlpmPkgName
    | RemovePkgError AlpmPkgName
    | TransCommitFileconflictError [AlpmFileconflict]
    | TransCommitOtherError
    | TransCommitTextError [String]
    | TransPrepareConflictError [AlpmConflict]
    | TransPrepareDepmissingError [AlpmDepmissing]
    | TransPrepareOtherError
    | TransPrepareTextError [String]
    deriving (Eq, Show)

instance Exception AlpmTransactionError
