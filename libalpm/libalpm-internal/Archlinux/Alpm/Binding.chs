{-# LANGUAGE RecordWildCards #-}

module Archlinux.Alpm.Binding where

import Prelude

import Control.Exception (Exception (..), bracket, handle)
import Control.Monad ((<=<), unless)
import Control.Monad (foldM)
import Control.Monad.Catch (throwM)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Coerce (coerce)
--import Data.Text.Display (Display (..))
import Data.Typeable (Typeable)
import Foreign hiding (void)
import Foreign.C
import System.OsPath (OsPath)

import Data.ByteString qualified as ByteString
import Data.ByteString.Short qualified as ShortByteString
import Data.ByteString.Short.Internal qualified as ShortByteString.Internal
import System.OsString.Internal.Types qualified as OsString.Internal

import Archlinux.Alpm.Names

#include <alpm.h>

{#context prefix = "alpm" add prefix = "" #}

{#enum alpm_db_usage_t          as DbUsage          {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_depmod_t            as Depmod           {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_errno_t             as ErrNo            {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_event_type_t        as EventType        {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_hook_when_t         as HookWhen         {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_fileconflicttype_t  as Fileconflicttype {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_package_operation_t as PackageOperation {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_pkgfrom_t           as Pkgfrom          {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_pkgreason_t         as Pkgreason        {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_question_type_t     as QuestionType     {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_siglevel_t          as Siglevel         {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_transflag_t         as Transflag        {underscoreToCase} deriving (Eq, Show) #}

{#pointer *alpm_backup_t       as BackupPtr        newtype         #}
{#pointer  alpm_cb_event       as CbEventFunPtr                    #}
{#pointer  alpm_cb_question    as CbQuestionFunPtr                 #}
{#pointer *alpm_conflict_t     as ConflictPtr      -> Conflict     #}
{#pointer *alpm_db_t           as DbPtr            newtype         #}
{#pointer *alpm_depend_t       as DependPtr        -> Depend       #}
{#pointer *alpm_depmissing_t   as DepmissingPtr    -> Depmissing   #}
{#pointer *alpm_errno_t        as ErrNoPtr                         #}
{#pointer *alpm_event_t        as EventPtr         -> Event        #}
{#pointer *alpm_file_t         as FilePtr          newtype         #}
{#pointer *alpm_fileconflict_t as FileconflictPtr  -> Fileconflict #}
{#pointer *alpm_filelist_t     as FilelistPtr      newtype         #}
{#pointer *alpm_group_t        as GroupPtr         -> Group        #}
{#pointer *alpm_handle_t       as HandlePtr        newtype         #}
{#pointer *alpm_list_t         as ListPtr                          #}
{#pointer *alpm_pkg_t          as PkgPtr           -> Pkg          #}
{#pointer *alpm_question_t     as QuestionPtr      -> Question     #}



data Error a = Error ErrNo a
    deriving (Eq, Show)

instance (Show a, Typeable a) => Exception (Error a)

data UnknownError = UnknownError
    deriving (Eq, Show)

instance Exception UnknownError

--------------------------------------------------------------------------------
-- Handles
--------------------------------------------------------------------------------

withAlpm :: OsPath -> OsPath -> (HandlePtr -> IO a) -> IO a
withAlpm rootDir dbDir action = bracket (initialize rootDir dbDir) release action

initialize :: OsPath -> OsPath -> IO HandlePtr
initialize rootDir dbDir = do
    rootDir' <- newCStringOsPath rootDir
    dbDir' <- newCStringOsPath dbDir
    bracket calloc free $ \e -> do
        h@(HandlePtr p) <- {#call alpm_initialize #} rootDir' dbDir' e
        if p == nullPtr
            then do
                e' <- peek e
                throwM $ Error (toEnum $ fromIntegral e') InitializationError 
            else pure h

release :: HandlePtr -> IO Int
release = fmap fromIntegral . {#call alpm_release #}

syncSysupgrade :: HandlePtr -> Bool -> IO ()
syncSysupgrade h = throwError h . {#call alpm_sync_sysupgrade #} h . fromInteger . fromBool

optionSetEventCb :: HandlePtr -> (Event -> IO ()) -> IO ()
optionSetEventCb h cb = do
    cb' <- createCbEventFunPtr $ \_ p -> do
        evt <- peek (castPtr p)
        cb evt
    throwError' h SetEventCbError $
        {#call alpm_option_set_eventcb #} h cb' nullPtr

foreign import ccall "wrapper"
    createCbEventFunPtr :: (Ptr () -> Ptr () -> IO ()) -> IO CbEventFunPtr

data Event
    = Event EventType
    | EventHook' HookWhen
    | EventHookRun' Word Word HookName ShortByteString
    | EventOptdepRemoval' PkgPtr DependPtr
    | EventPacnewCreated' EventType OsPath
    | EventPacsaveCreated' EventType OsPath
    | EventPackageOperation' PackageOperation (Maybe PkgPtr) (Maybe PkgPtr)
    | EventScriptletInfo' ByteString
    deriving Show

--instance Display Event where
--    displayBuilder = displayBuilder . show  -- TODO

instance Storable Event where
    sizeOf _ = {#sizeof alpm_event_t #}
    alignment _ = {#alignof alpm_event_t #}
    peek p = do
        t <- toEnum . fromIntegral <$> {#get alpm_event_t->type #} p
        case t of
            EventHookStart -> EventHook' . toEnum . fromIntegral
                <$> {#get alpm_event_t->hook.when #} p
            EventHookRunStart -> EventHookRun'
                <$> (fromIntegral <$> {#get alpm_event_t->hook_run.position #} p)
                <*> (fromIntegral <$> {#get alpm_event_t->hook_run.total #} p)
                <*> (HookName <$> (ShortByteString.packCString =<< {#get alpm_event_t->hook_run.name #} p))
                <*> (ShortByteString.packCString =<< {#get alpm_event_t->hook_run.desc #} p)
            EventOptdepRemoval -> EventOptdepRemoval'
                <$> {#get alpm_event_t->optdep_removal.pkg #} p
                <*> {#get alpm_event_t->optdep_removal.optdep #} p
            EventPackageOperationStart -> do
                packageOp <- toEnum . fromIntegral
                    <$> {#get alpm_event_t->package_operation.operation #} p
                oldPkg <- {#get alpm_event_t->package_operation.oldpkg #} p
                newPkg <- {#get alpm_event_t->package_operation.newpkg #} p
                pure $ EventPackageOperation'
                    packageOp
                    (if oldPkg == nullPtr then Nothing else Just oldPkg)
                    (if newPkg == nullPtr then Nothing else Just newPkg)
            EventPacnewCreated -> EventPacnewCreated' t . coerce
                <$> (ShortByteString.packCString =<< {#get alpm_event_t->pacnew_created.file #} p)
            EventPacsaveCreated -> EventPacsaveCreated' t . coerce
                <$> (ShortByteString.packCString =<< {#get alpm_event_t->pacsave_created.file #} p)
            EventScriptletInfo -> EventScriptletInfo'
                <$> (ByteString.packCString =<< {#get alpm_event_t->scriptlet_info.line #} p)
            _ -> pure (Event t)
    poke _ _ = error "Storable.poke not implemented for Event !"

optionSetQuestionCb :: HandlePtr -> (Question -> IO ()) -> IO ()
optionSetQuestionCb h cb = do
    cb' <- createCbQuestionFunPtr $ \_ p -> do
        q <- peek (castPtr p)
        cb q
    throwError' h SetQuestionCbError $
        {#call alpm_option_set_questioncb #} h cb' nullPtr

foreign import ccall "wrapper"
    createCbQuestionFunPtr :: (Ptr () -> Ptr () -> IO ()) -> IO CbQuestionFunPtr

data Question
    = Question QuestionType
    | QuestionSelectProvider' DependPtr [PkgPtr] (Int -> IO ())

instance Storable Question where
    sizeOf _ = {#sizeof alpm_question_t #}
    alignment _ = {#alignof alpm_question_t #}
    peek p = do
        t <- toEnum . fromIntegral <$> {#get alpm_question_t->type #} p
        case t of
            QuestionSelectProvider -> do
                dep <- {#get alpm_question_t->select_provider.depend #} p
                ps <- {#get alpm_question_t->select_provider.providers #} p >>= fromList
                pure $ QuestionSelectProvider'
                    dep
                    ps
                    ({#set alpm_question_t->select_provider.use_index #} p . fromIntegral)
            _ -> pure $ Question t
    poke _ _ = error "Storable.poke not implemented for Question !"

data HandleError
    = InitializationError
    | SetEventCbError
    | SetQuestionCbError
    deriving (Eq, Show)

instance Exception HandleError

--------------------------------------------------------------------------------
-- Databases
--------------------------------------------------------------------------------

getLocaldb :: HandlePtr -> IO DbPtr
getLocaldb = {#call alpm_get_localdb #}

getSyncdbs :: HandlePtr -> IO [DbPtr]
getSyncdbs h = do
    res <- {#call alpm_get_syncdbs #} h
    map DbPtr <$> fromList res

registerSyncdb :: HandlePtr -> DatabaseName -> [Siglevel] -> IO DbPtr
registerSyncdb h (DatabaseName name) siglevel = do
    name' <- newCStringSBS name
    {#call alpm_register_syncdb #} h name' (makeBitmask siglevel)

dbGetName :: DbPtr -> IO DatabaseName
dbGetName db = do
    name <- {#call alpm_db_get_name #} db
    DatabaseName <$> ShortByteString.packCString name

dbSetUsage :: HandlePtr -> DbPtr -> [DbUsage] -> IO ()
dbSetUsage h db xs = 
    throwErrorM' h (DbSetUsageError xs <$> dbGetName db) $
        {#call alpm_db_set_usage #} db (makeBitmask xs)

dbGetServers :: DbPtr -> IO [ShortByteString]
dbGetServers db = do
    res <- {#call alpm_db_get_servers #} db
    peekStringList res

dbAddServer :: HandlePtr -> DbPtr -> ShortByteString -> IO ()
dbAddServer h db server = do
    server' <- newCStringSBS server
    throwErrorM' h (DbAddServerError server <$> dbGetName db) $
        {#call alpm_db_add_server #} db server'

dbSetServers :: HandlePtr -> DbPtr -> [ShortByteString] -> IO ()
dbSetServers h db xss = do
    xss' <- traverse newCStringSBS xss
    xs <- toList xss'
    throwErrorM' h (DbSetServersError xss <$> dbGetName db) $
        {#call alpm_db_set_servers #} db xs

dbRemoveServer :: HandlePtr -> DbPtr -> ShortByteString -> IO ()
dbRemoveServer h db server = do
    server' <- newCStringSBS server
    throwErrorM' h (DbRemoveServerError server <$> dbGetName db) $
        {#call alpm_db_remove_server #} db server'

dbGetGroup :: DbPtr -> GroupName -> IO Group
dbGetGroup db (GroupName name) = do
    name' <- newCStringSBS name
    peek =<< {#call alpm_db_get_group #} db name'

dbGetGroupcache :: DbPtr -> IO [Group]
dbGetGroupcache db = do
    groups <- {#call alpm_db_get_groupcache #} db
    peekList groups

dbGetPkg :: DbPtr -> PackageName -> IO (Maybe PkgPtr)
dbGetPkg h (PackageName name) = do
    name' <- newCStringSBS name
    ptr <- {#call alpm_db_get_pkg #} h name'
    pure $ if ptr == nullPtr
        then Nothing
        else Just ptr

dbGetPkgcache :: DbPtr -> IO [PkgPtr]
dbGetPkgcache db = do
    pkgs <- {#call alpm_db_get_pkgcache #} db
    fromList pkgs

dbSearch :: HandlePtr -> DbPtr -> [ShortByteString] -> IO [PkgPtr]
dbSearch h db patterns = 
    withCStrings patterns $ \patterns' -> do
        patterns'' <- toList patterns'
        alloca $ \ret -> do
            throwErrorM' h (DbSearchError patterns <$> dbGetName db) $
                {#call alpm_db_search #} db patterns'' ret
            fromList =<< peek ret

dbUpdate :: HandlePtr -> [DbPtr] -> Bool -> IO UpdateResult
dbUpdate _ [] _ = pure DbUpdateSkipped
dbUpdate h dbs doForce = do
    dbs' <- toList $ map unDbPtr dbs
    ec <- {#call alpm_db_update #} h dbs' (fromBool doForce)
    case ec of
        0 -> pure DbUpdated
        1 -> pure DbUpToDate
        _ -> do
            err <- errno h
            ns <- traverse dbGetName dbs
            throwM $ Error err $ DbUpdateError ns
    where
        unDbPtr (DbPtr p) = p

findDbsSatisfier :: HandlePtr -> [DbPtr] -> ShortByteString -> IO (Maybe PkgPtr)
findDbsSatisfier _ [] _ = pure Nothing
findDbsSatisfier h dbs sbs = 
    withList (map unDbPtr dbs) $ \dbs' ->
    ShortByteString.useAsCString sbs $ \sbs' -> do
        p <- {#call alpm_find_dbs_satisfier #} h dbs' sbs'
        pure $ if p == nullPtr
            then Nothing
            else Just p
    where
        unDbPtr (DbPtr p) = p

findDbsSatisfier' :: HandlePtr -> [DbPtr] -> Depend -> IO (Maybe PkgPtr)
findDbsSatisfier' _ [] _ = pure Nothing
findDbsSatisfier' h dbs x = alloca $ \p -> do
    poke p x
    xs <- depComputeString p
    --depFree p
    findDbsSatisfier h dbs xs

data UpdateResult
    = DbUpdated
    | DbUpdateSkipped
    | DbUpToDate
    deriving (Eq, Show)

data DbError
    = DbAddServerError ShortByteString DatabaseName
    | DbSetServersError [ShortByteString] DatabaseName
    | DbSetUsageError [DbUsage] DatabaseName
    | DbRemoveServerError ShortByteString DatabaseName
    | DbSearchError [ShortByteString] DatabaseName
    | DbUpdateError [DatabaseName]
    deriving (Eq, Show)

instance Exception DbError

--------------------------------------------------------------------------------
-- Packages
--------------------------------------------------------------------------------

data Pkg = Pkg
    { alpmPkgName :: PackageName
    , alpmPkgVersion :: Version
    } deriving (Eq, Show)

instance Storable Pkg where
    sizeOf _ = error "Storable.sizeOf not implemented for Pkg !"
    alignment _ = error "Storable.alignment not implemented for Pkg !"
    peek p = Pkg
        <$> pkgGetName p
        <*> (parseVersion . unVersionString =<< pkgGetVersion p)
    poke _ _ = error "Storable.poke not implemented for Pkg !"

pkgLoad :: HandlePtr -> Bool -> [Siglevel] -> FilePath -> IO PkgPtr
pkgLoad h full siglevel fp = do
    fp' <- newCString fp
    res <- malloc
    throwError' h (PkgLoadError fp) $
        {#call alpm_pkg_load #} h fp' (fromInteger $ fromBool full) (makeBitmask siglevel) res
    peek res

pkgFree :: HandlePtr -> PkgPtr -> IO ()
pkgFree h pkg = 
    throwErrorM' h (PkgFreeError <$> pkgGetName pkg) $
        {#call alpm_pkg_free #} pkg

pkgGetFilename :: PkgPtr -> IO FilePath
pkgGetFilename pkg = do
    res <- {#call alpm_pkg_get_filename #} pkg
    peekCString res

pkgGetOrigin :: PkgPtr -> IO Pkgfrom
pkgGetOrigin = fmap (toEnum . fromIntegral) . {#call alpm_pkg_get_origin #}

pkgGetDb :: PkgPtr -> IO (Maybe DbPtr)
pkgGetDb pkg = do
    res@(DbPtr p) <- {#call alpm_pkg_get_db #} pkg
    pure $ if p == nullPtr
        then Nothing
        else Just res

pkgGetDepends :: PkgPtr -> IO [Depend]
pkgGetDepends pkg = do
    res <- {#call alpm_pkg_get_depends #} pkg
    peekList res

pkgGetName :: PkgPtr -> IO PackageName
pkgGetName pkg = do
    res <- {#call alpm_pkg_get_name #} pkg
    PackageName <$> ShortByteString.packCString res

pkgGetProvides :: PkgPtr -> IO [Depend]
pkgGetProvides pkg = do
    res <- {#call alpm_pkg_get_provides #} pkg
    peekList res

pkgGetVersion :: PkgPtr -> IO VersionString
pkgGetVersion pkg = do
    res <- {#call alpm_pkg_get_version #} pkg
    VersionString <$> ShortByteString.packCString res

pkgSetReason :: HandlePtr -> PkgPtr -> Pkgreason -> IO ()
pkgSetReason h pkg reason = 
    throwErrorM' h (PkgSetReasonError reason <$> pkgGetName pkg) $
        {#call alpm_pkg_set_reason #} pkg (fromIntegral (fromEnum reason))

data PkgError
    = PkgFreeError PackageName
    | PkgLoadError FilePath
    | PkgSetReasonError Pkgreason PackageName
    deriving (Eq, Show)

instance Exception PkgError

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

data Depend = Depend
    { alpmDependName :: PackageName
    , alpmDependConstraint :: Constraint
    } deriving (Eq, Show)

instance Storable Depend where
    sizeOf _ = {#sizeof alpm_depend_t #}
    alignment _ = {#alignof alpm_depend_t #}
    peek p = Depend
        <$> (PackageName <$> (ShortByteString.packCString =<< {#get alpm_depend_t->name #} p))
        <*> peekConstraint p
    poke p x = do
        n <- newCStringSBS . unPackageName $ alpmDependName x
        {#set alpm_depend_t->name #} p n
        pokeConstraint               p $ alpmDependConstraint x

depComputeString :: DependPtr -> IO ShortByteString
depComputeString dep = ShortByteString.packCString =<< {#call alpm_dep_compute_string #} dep

depFromString :: ShortByteString -> IO DependPtr
depFromString sbs = ShortByteString.useAsCString sbs {#call alpm_dep_from_string #}

depFree :: DependPtr -> IO ()
depFree = {#call alpm_dep_free #}

data Constraint
    = ConstraintAny
    | ConstraintLT Version
    | ConstraintLE Version
    | ConstraintEQ Version
    | ConstraintGE Version
    | ConstraintGT Version
    deriving (Eq, Show)

constraintToDepmod :: Constraint -> Depmod
constraintToDepmod ConstraintAny  = DepModAny
constraintToDepmod ConstraintLT{} = DepModLt
constraintToDepmod ConstraintLE{} = DepModLe
constraintToDepmod ConstraintEQ{} = DepModEq
constraintToDepmod ConstraintGE{} = DepModGe
constraintToDepmod ConstraintGT{} = DepModGt

constraintVersion :: Constraint -> Maybe Version
constraintVersion ConstraintAny    = Nothing
constraintVersion (ConstraintLT v) = Just v
constraintVersion (ConstraintLE v) = Just v
constraintVersion (ConstraintEQ v) = Just v
constraintVersion (ConstraintGE v) = Just v
constraintVersion (ConstraintGT v) = Just v

peekConstraint :: Ptr Depend -> IO Constraint
peekConstraint p = do
    mversion <-
        peekMaybe (parseVersion <=< ShortByteString.packCString)
            =<< {#get alpm_depend_t->version #} p
    depmod <- toEnum . fromIntegral <$> {#get alpm_depend_t->mod #} p
    pure $ case (mversion, depmod) of
        (Nothing, DepModAny) -> ConstraintAny
        (Just version, DepModEq) -> ConstraintEQ version
        (Just version, DepModLe) -> ConstraintLE version
        (Just version, DepModGe) -> ConstraintGE version
        (Just version, DepModLt) -> ConstraintLT version
        (Just version, DepModGt) -> ConstraintGT version
        _ -> error $ "Illegal version/depmod compination: " <> show mversion <> " " <> show depmod

pokeConstraint :: Ptr Depend -> Constraint -> IO ()
pokeConstraint p constraint = do
    let
        depmod = constraintToDepmod constraint

    v <- case constraintVersion constraint of
        Nothing -> pure nullPtr
        Just version  -> do
            let
                VersionString versionString = showVersion version
            newCStringSBS versionString
    {#set alpm_depend_t->version #} p v
    {#set alpm_depend_t->mod #} p (fromIntegral (fromEnum depmod))

--------------------------------------------------------------------------------
-- Groups
--------------------------------------------------------------------------------

data Group = Group
    { alpmGroupName :: GroupName
    , alpmGroupPackages :: [PkgPtr]
    } deriving Show

instance Storable Group where
    sizeOf _ = {#sizeof alpm_group_t #}
    alignment _ = {#alignof alpm_group_t #}
    peek p = Group
        <$> (GroupName <$> (ShortByteString.packCString =<< {#get alpm_group_t->name #} p))
        <*> (fromList =<< {#get alpm_group_t->packages #} p)
    poke _ _ = error "Storable.poke not implemented for Group !"

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

withTrans :: HandlePtr -> [Transflag] -> IO a -> IO a
withTrans h flags f = bracket (transInit h flags) (\_ -> transRelease h) (\_ -> f)

transInit :: HandlePtr -> [Transflag] -> IO ()
transInit h flags = 
    throwError h $
        {#call alpm_trans_init #} h $ makeBitmask flags

transPrepare :: HandlePtr -> IO ()
transPrepare h = 
    alloca $ \p -> do
        let onErr :: ErrNo -> UnknownError -> IO TransactionError
            onErr err _ = do
                p' <- peek p
                case err of
                    ErrPkgInvalidArch -> TransPrepareTextError
                        <$> peekStringListFree p'
                    ErrUnsatisfiedDeps -> TransPrepareDepmissingError
                        <$> peekListFree {#call alpm_depmissing_free #} p'
                    ErrConflictingDeps -> TransPrepareConflictError
                        <$> peekListFree {#call alpm_conflict_free #} p'
                    _ -> pure TransPrepareOtherError
        modifyErrorM onErr $ throwError h $ {#call alpm_trans_prepare #} h p
        p' <- peek p
        {#call alpm_list_free #} p'

transCommit :: HandlePtr -> IO ()
transCommit h = 
    alloca $ \p -> do
        let onErr :: ErrNo -> UnknownError -> IO TransactionError
            onErr err _ = do
                p' <- peek p
                case err of
                    ErrFileConflicts -> TransCommitFileconflictError
                        <$> peekListFree {#call alpm_fileconflict_free #} p'
                    ErrPkgInvalid -> TransCommitTextError
                        <$> peekStringListFree p'
                    ErrPkgInvalidChecksum -> TransCommitTextError
                        <$> peekStringListFree p'
                    ErrPkgInvalidSig -> TransCommitTextError
                        <$> peekStringListFree p'
                    _ -> pure TransCommitOtherError
        modifyErrorM onErr $ throwError h $ {#call alpm_trans_commit #} h p

transInterrupt :: HandlePtr -> IO ()
transInterrupt h = throwError h $ {#call alpm_trans_interrupt #} h

transRelease :: HandlePtr -> IO ()
transRelease h = throwError h $ {#call alpm_trans_release #} h

transGetAdd :: HandlePtr -> IO [PkgPtr]
transGetAdd h = do
    res <- {#call alpm_trans_get_add #} h
    fromList res

transGetRemove :: HandlePtr -> IO [PkgPtr]
transGetRemove h = do
    res <- {#call alpm_trans_get_remove #} h
    fromList res

addPkg :: HandlePtr -> PkgPtr -> IO ()
addPkg h pkg = 
    throwErrorM' h (AddPkgError <$> pkgGetName pkg) $
        {#call alpm_add_pkg #} h pkg

removePkg :: HandlePtr -> PkgPtr -> IO ()
removePkg h pkg = 
    throwErrorM' h (RemovePkgError <$> pkgGetName pkg) $
        {#call alpm_remove_pkg #} h pkg

data TransactionError
    = AddPkgError PackageName
    | RemovePkgError PackageName
    | TransCommitFileconflictError [Fileconflict]
    | TransCommitOtherError
    | TransCommitTextError [ShortByteString]
    | TransPrepareConflictError [Conflict]
    | TransPrepareDepmissingError [Depmissing]
    | TransPrepareOtherError
    | TransPrepareTextError [ShortByteString]
    deriving (Eq, Show)

instance Exception TransactionError

data Conflict = Conflict
    { alpmConflictPkg1   :: Pkg
    , alpmConflictPkg2   :: Pkg
    , alpmConflictReason :: Depend
    } deriving (Eq, Show)

instance Storable Conflict where
    sizeOf _ = {#sizeof alpm_conflict_t #}
    alignment _ = {#alignof alpm_conflict_t #}
    peek p = do
        alpmConflictPkg1 <-
            peek =<< {#get alpm_conflict_t->package1 #} p
        alpmConflictPkg2 <-
            peek =<< {#get alpm_conflict_t->package2 #} p
        alpmConflictReason <-
            peek =<< {#get alpm_conflict_t->reason #} p
        pure Conflict {..}
    poke _ _ = error "Storable.poke not implemented for Conflict !"

data Depmissing = Depmissing
    { alpmDepmissingTarget     :: PackageName
    , alpmDepmissingDepend     :: Depend
    , alpmDepmissingCausingPkg :: Maybe PackageName
    } deriving (Eq, Show)

instance Storable Depmissing where
    sizeOf _ = {#sizeof alpm_depmissing_t #}
    alignment _ = {#alignof alpm_depmissing_t #}
    peek p = Depmissing
        <$> (PackageName <$> (ShortByteString.packCString =<< {#get alpm_depmissing_t->target #} p))
        <*> (peek =<< {#get alpm_depmissing_t->depend #} p)
        <*> (peekMaybe (fmap PackageName . ShortByteString.packCString) =<< {#get alpm_depmissing_t->causingpkg #} p)
    poke _ _ = error "Storable.poke not implemented for Depmissing !"

data Fileconflict = Fileconflict
    { alpmFileconflictTarget :: PackageName
    , alpmFileconflictType :: Fileconflicttype
    , alpmFileconflictFile :: OsPath
    , alpmFileconflictCtarget :: Maybe PackageName
    } deriving (Eq, Show)

instance Storable Fileconflict where
    sizeOf _ = {#sizeof alpm_fileconflict_t #}
    alignment _ = {#alignof alpm_fileconflict_t #}
    peek p = Fileconflict
        <$> (PackageName <$> (ShortByteString.packCString =<< {#get alpm_fileconflict_t->target #} p))
        <*> (toEnum . fromIntegral <$> {#get alpm_fileconflict_t->type #} p)
        <*> (coerce <$> (ShortByteString.packCString =<< {#get alpm_fileconflict_t->file #} p))
        <*> (peekMaybe (fmap PackageName . ShortByteString.packCString) =<< {#get alpm_fileconflict_t->ctarget #} p)
    poke _ _ = error "Storable.poke not implemented for Fileconflict !"

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

errno :: HandlePtr -> IO ErrNo
errno h = toEnum . fromIntegral <$> {#call alpm_errno #} h

strerror :: ErrNo -> IO String
strerror e = do
    xs <- {#call alpm_strerror #} $ fromIntegral $ fromEnum e
    peekCString xs

throwError :: HandlePtr -> IO CInt -> IO ()
throwError h = throwError' h UnknownError

throwError' :: (Show e, Typeable e) => HandlePtr -> e -> IO CInt -> IO ()
throwError' h x = throwErrorM' h (pure x)

throwErrorM' :: (Show e, Typeable e) => HandlePtr -> IO e -> IO CInt -> IO ()
throwErrorM' h x k = do
    ec <- k
    unless (ec == 0) $
        throwM =<< (Error <$> errno h <*> x)

modifyErrorM :: (Show e, Show e', Typeable e, Typeable e') => (ErrNo -> e -> IO e') -> IO c -> IO c
modifyErrorM f = handle (\(Error err x) -> f err x >>= throwM . Error err)

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

fromList :: ListPtr -> IO [Ptr a]
fromList = traverseList (pure . (:[]))

peekList :: Storable a => ListPtr -> IO [a]
peekList = traverseList $ \p' -> do
    x <- peek p'
    pure [x]

peekListFree :: Storable a => (Ptr a -> IO ()) -> ListPtr -> IO [a]
peekListFree freeF p = freeListAfter p $
    traverseList $ \p' -> do
        x <- peek p'
        freeF p'
        pure [x]

peekStringList :: ListPtr -> IO [ShortByteString]
peekStringList = traverseList $ \p' -> do
    x <- ShortByteString.packCString p'
    pure [x]

peekStringListFree :: ListPtr -> IO [ShortByteString]
peekStringListFree p = do
    res <- traverseList (\p' -> do
        x <- ShortByteString.packCString p'
        free p'
        pure [x]
        ) p
    {#call alpm_list_free #} p
    pure res

toList :: [Ptr a] -> IO ListPtr
toList = foldM (\memo -> {#call alpm_list_add #} memo . castPtr) nullPtr

withList :: [Ptr a] -> (ListPtr -> IO b) -> IO b
withList xs = bracket (toList xs) {#call alpm_list_free #}

freeListAfter :: ListPtr -> (ListPtr -> IO a) -> IO a
freeListAfter p k = do
    res <- k p
    {#call alpm_list_free #} p
    pure res

traverseList :: Monoid m => (Ptr a -> IO m) -> ListPtr -> IO m
traverseList f = go id
    where
        go !contF xs
            | xs == nullPtr = pure (contF mempty)
            | otherwise = do
                x <- {#get alpm_list_t->data #} xs
                xs' <- {#get alpm_list_t->next #} xs
                y <- f (castPtr x)
                go (contF . (y <>)) xs'

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

withCStrings :: [ShortByteString] -> ([CString] -> IO a) -> IO a
withCStrings xss f = go [] xss
    where
        go !acc []     = f (reverse acc)
        go !acc (x:xs) = ShortByteString.useAsCString x (\x' -> go (x':acc) xs)

peekMaybe :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
peekMaybe f p
    | p == nullPtr = pure Nothing
    | otherwise    = Just <$> f p

pokeMaybe :: (Storable a) => (Ptr a -> a -> IO ()) -> Maybe a -> IO (Ptr a)
pokeMaybe _ Nothing  = pure nullPtr
pokeMaybe f (Just x) = do
    p <- malloc
    f p x
    pure p

makeBitmask :: Enum a => [a] -> CInt
makeBitmask = foldl' (\memo x -> memo .|. fromIntegral (fromEnum x)) zeroBits

newCStringOsPath :: OsPath -> IO CString
newCStringOsPath = newCStringSBS . coerce

newCStringSBS :: ShortByteString -> IO CString
newCStringSBS sbs = do
    buf <- mallocBytes (l + 1)
    ShortByteString.Internal.copyToPtr sbs 0 buf l
    pokeByteOff buf l (0 :: Word8)
    pure buf
    where
        l = ShortByteString.length sbs
