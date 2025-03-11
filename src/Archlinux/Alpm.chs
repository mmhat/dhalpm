{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Archlinux.Alpm
    ( AlpmConstraint(..)
    , AlpmDepend(..)
    , AlpmDependPtr
    , AlpmDepmissing(..)
    , AlpmDepmod(..)
    , AlpmGroup(..)
    , AlpmError(..)
    , AlpmUnknownError(..)
    , AlpmDbError(..)
    , AlpmPkgError(..)
    , AlpmTransactionError(..)
    , AlpmErrNo(..)
    , AlpmEvent(..)
    , AlpmEventType(..)
    , AlpmHandlePtr
    , AlpmDbPtr
    , AlpmPkg(..)
    , AlpmPkgfrom(..)
    , AlpmPkgName(..)
    , AlpmPkgPtr
    , AlpmPkgreason(..)
    , AlpmQuestion(..)
    , AlpmSiglevel(..)
    , AlpmTransflag(..)
    , AlpmVersion(..)
    , UpdateResult(..)
    , AlpmPkgNameParseException(..)
    , AlpmVersionParseException(..)
    --
    , emptyAlpmPkgName
    , errno
    , strerror
    , withAlpm
    , initialize
    , release
    , getLocaldb
    , getSyncdbs
    , registerSyncdb
    , pkgLoad
    , optionSetEventCb
    , optionSetQuestionCb
    , dbSetUsage
    , dbGetServers
    , dbAddServer
    , dbSetServers
    , dbRemoveServer
    , dbGetGroup
    , dbGetGroupcache
    , dbGetPkg
    , dbGetPkgcache
    , dbSearch
    , dbUpdate
    , findDbsSatisfier
    , findDbsSatisfier'
    , pkgFree
    , pkgGetDb
    , pkgGetDepends
    , pkgGetFilename
    , pkgGetName
    , pkgGetOrigin
    , pkgGetProvides
    , pkgGetVersion
    , pkgSetReason
    , withTrans
    , transPrepare
    , transCommit
    , transInterrupt
    , transRelease
    , transGetAdd
    , transGetRemove
    , syncSysupgrade
    , addPkg
    , removePkg
    , depComputeString
    , depFromString
    , depFree
    , vercmp
    , vercmp'
    --
    , fromAlpmList
    , toAlpmList
    , withAlpmList
    , parseAlpmPkgName
    , parseAlpmVersion
    , pkgNameP
    , alpmVersionP
    , showAlpmVersion
    -- Testing only
    , dbGetName
    , withCStrings
    ) where

import RIO hiding (to)

import qualified Data.Attoparsec.Text as AT
import Prelude (Enum(..))
import RIO.Char (isAlphaNum)
import qualified RIO.Text as T
import Foreign hiding (void)
import Foreign.C
import qualified System.IO.Unsafe as Unsafe

default (Text)



#include <alpm.h>

{#context prefix = "alpm" add prefix = "Alpm" #}

{#enum alpm_db_usage_t          as AlpmDbUsage          {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_depmod_t            as AlpmDepmod           {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_errno_t             as AlpmErrNo            {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_event_type_t        as AlpmEventType        {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_hook_when_t         as AlpmHookWhen         {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_fileconflicttype_t  as AlpmFileconflicttype {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_package_operation_t as AlpmPackageOperation {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_pkgfrom_t           as AlpmPkgfrom          {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_pkgreason_t         as AlpmPkgreason        {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_question_type_t     as AlpmQuestionType     {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_siglevel_t          as AlpmSiglevel         {underscoreToCase} deriving (Eq, Show) #}
{#enum alpm_transflag_t         as AlpmTransflag        {underscoreToCase} deriving (Eq, Show) #}

{#pointer *alpm_backup_t       as AlpmBackupPtr        newtype         #}
{#pointer  alpm_cb_event       as AlpmCbEventFunPtr                    #}
{#pointer  alpm_cb_question    as AlpmCbQuestionFunPtr                 #}
{#pointer *alpm_conflict_t     as AlpmConflictPtr                      #}
{#pointer *alpm_db_t           as AlpmDbPtr            newtype         #}
{#pointer *alpm_depend_t       as AlpmDependPtr        -> AlpmDepend   #}
{#pointer *alpm_depmissing_t   as AlpmDepmissingPtr                    #}
{#pointer *alpm_errno_t        as AlpmErrNoPtr                         #}
{#pointer *alpm_event_t        as AlpmEventPtr         -> AlpmEvent    #}
{#pointer *alpm_file_t         as AlpmFilePtr          newtype         #}
{#pointer *alpm_fileconflict_t as AlpmFileconflictPtr                  #}
{#pointer *alpm_filelist_t     as AlpmFilelistPtr      newtype         #}
{#pointer *alpm_group_t        as AlpmGroupPtr                         #}
{#pointer *alpm_handle_t       as AlpmHandlePtr        newtype         #}
{#pointer *alpm_list_t         as AlpmListPtr                          #}
{#pointer *alpm_pkg_t          as AlpmPkgPtr           newtype         #}
{#pointer *alpm_question_t     as AlpmQuestionPtr      -> AlpmQuestion #}

deriving instance Show AlpmPkgPtr
deriving instance Storable AlpmPkgPtr



data AlpmError a = AlpmError AlpmErrNo a
    deriving (Eq, Show)

instance (Show a, Typeable a) => Exception (AlpmError a)

data AlpmUnknownError = UnknownAlpmError
    deriving (Eq, Show)

instance Exception AlpmUnknownError

--------------------------------------------------------------------------------
-- Handles
--------------------------------------------------------------------------------

withAlpm :: MonadUnliftIO m => FilePath -> FilePath -> (AlpmHandlePtr -> m a) -> m a
withAlpm rootDir dbDir = bracket (initialize rootDir dbDir) release

initialize :: MonadIO m => FilePath -> FilePath -> m AlpmHandlePtr
initialize rootDir dbDir = liftIO $ do
    rootDir' <- newCString rootDir
    dbDir' <- newCString dbDir
    bracket calloc free $ \e -> do
        h@(AlpmHandlePtr p) <- {#call alpm_initialize #} rootDir' dbDir' e
        if p == nullPtr
            then do
                e' <- peek e
                throwM $ AlpmError (toEnum $ fromIntegral e') InitializationError 
            else return h

release :: MonadIO m => AlpmHandlePtr -> m Int
release = fmap fromIntegral . liftIO . {#call alpm_release #}

syncSysupgrade :: MonadIO m => AlpmHandlePtr -> Bool -> m ()
syncSysupgrade h = liftIO . throwAlpmError h . {#call alpm_sync_sysupgrade #} h . fromInteger . fromBool

optionSetEventCb :: MonadUnliftIO m => AlpmHandlePtr -> (AlpmEvent -> m ()) -> m ()
optionSetEventCb h cb = do
    u <- askUnliftIO
    liftIO $ do
        cb' <- createAlpmCbEventFunPtr $ \_ p -> unliftIO u $ do
            evt <- liftIO $ peek $ castPtr p
            cb evt
        throwAlpmError' h SetEventCbError $
            {#call alpm_option_set_eventcb #} h cb' nullPtr

foreign import ccall "wrapper"
    createAlpmCbEventFunPtr :: (Ptr () -> Ptr () -> IO ()) -> IO AlpmCbEventFunPtr

data AlpmEvent
    = AlpmEvent AlpmEventType
    | AlpmEventHook' AlpmHookWhen
    | AlpmEventHookRun' Natural Natural String String
    | AlpmEventOptdepRemoval' AlpmPkgPtr AlpmDependPtr
    | AlpmEventPacfileCreated' AlpmEventType FilePath
    | AlpmEventPackageOperation' AlpmPackageOperation (Maybe AlpmPkgPtr) (Maybe AlpmPkgPtr)
    | AlpmEventScriptletInfo' String  -- TODO: Text
    deriving Show

instance Display AlpmEvent where
    display = displayShow  -- TODO

instance Storable AlpmEvent where
    sizeOf _ = {#sizeof alpm_event_t #}
    alignment _ = {#alignof alpm_event_t #}
    peek p = do
        t <- toEnum . fromIntegral <$> {#get alpm_event_t->type #} p
        case t of
            AlpmEventHookStart -> AlpmEventHook' . toEnum . fromIntegral
                <$> {#get alpm_event_t->hook.when #} p
            AlpmEventHookRunStart -> AlpmEventHookRun'
                <$> (fromIntegral <$> {#get alpm_event_t->hook_run.position #} p)
                <*> (fromIntegral <$> {#get alpm_event_t->hook_run.total #}    p)
                <*> (                 {#get alpm_event_t->hook_run.name #}     p >>= peekCString)
                <*> (                 {#get alpm_event_t->hook_run.desc #}     p >>= peekCString)
            AlpmEventOptdepRemoval -> AlpmEventOptdepRemoval'
                <$> {#get alpm_event_t->optdep_removal.pkg #} p
                <*> {#get alpm_event_t->optdep_removal.optdep #} p
            AlpmEventPackageOperationStart -> do
                packageOp <- toEnum . fromIntegral
                    <$> {#get alpm_event_t->package_operation.operation #} p
                oldPkg@(AlpmPkgPtr oldPkg') <- {#get alpm_event_t->package_operation.oldpkg #} p
                newPkg@(AlpmPkgPtr newPkg') <- {#get alpm_event_t->package_operation.newpkg #} p
                return $ AlpmEventPackageOperation'
                    packageOp
                    (if oldPkg' == nullPtr then Nothing else Just oldPkg)
                    (if newPkg' == nullPtr then Nothing else Just newPkg)
            AlpmEventPacnewCreated -> AlpmEventPacfileCreated' t
                <$> ({#get alpm_event_t->pacnew_created.file #}  p >>= peekCString)
            AlpmEventPacsaveCreated -> AlpmEventPacfileCreated' t
                <$> ({#get alpm_event_t->pacsave_created.file #} p >>= peekCString)
            AlpmEventScriptletInfo -> AlpmEventScriptletInfo'
                <$> ({#get alpm_event_t->scriptlet_info.line #}  p >>= peekCString)
            _ -> return $ AlpmEvent t
    poke _ _ = error "Storable.poke not implemented for AlpmEvent !"

optionSetQuestionCb :: MonadUnliftIO m => AlpmHandlePtr -> (AlpmQuestion -> m ()) -> m ()
optionSetQuestionCb h cb = do
    u <- askUnliftIO
    liftIO $ do
        cb' <- createAlpmCbQuestionFunPtr $ \_ p -> unliftIO u $ do
            q <- liftIO $ peek $ castPtr p
            cb q
        throwAlpmError' h SetQuestionCbError $
            {#call alpm_option_set_questioncb #} h cb' nullPtr

foreign import ccall "wrapper"
    createAlpmCbQuestionFunPtr :: (Ptr () -> Ptr () -> IO ()) -> IO AlpmCbQuestionFunPtr

data AlpmQuestion
    = AlpmQuestion AlpmQuestionType
    | AlpmQuestionSelectProvider' AlpmDependPtr [AlpmPkgPtr] (Int -> IO ())

instance Storable AlpmQuestion where
    sizeOf _ = {#sizeof alpm_question_t #}
    alignment _ = {#alignof alpm_question_t #}
    peek p = do
        t <- toEnum . fromIntegral <$> {#get alpm_question_t->type #} p
        case t of
            AlpmQuestionSelectProvider -> do
                dep <- {#get alpm_question_t->select_provider.depend #} p
                ps <- {#get alpm_question_t->select_provider.providers #} p >>= fromAlpmList
                return $ AlpmQuestionSelectProvider'
                    dep
                    (map AlpmPkgPtr ps)
                    ({#set alpm_question_t->select_provider.use_index #} p . fromIntegral)
            _ -> return $ AlpmQuestion t
    poke _ _ = error "Storable.poke not implemented for AlpmQuestion !"

data AlpmHandleError
    = InitializationError
    | SetEventCbError
    | SetQuestionCbError
    deriving (Eq, Show)

instance Exception AlpmHandleError

--------------------------------------------------------------------------------
-- Databases
--------------------------------------------------------------------------------

getLocaldb :: MonadIO m => AlpmHandlePtr -> m AlpmDbPtr
getLocaldb = liftIO . {#call alpm_get_localdb #}

getSyncdbs :: MonadIO m => AlpmHandlePtr -> m [AlpmDbPtr]
getSyncdbs h = liftIO $ do
    res <- {#call alpm_get_syncdbs #} h
    map AlpmDbPtr <$> fromAlpmList res

registerSyncdb :: MonadIO m => AlpmHandlePtr -> String -> [AlpmSiglevel] -> m AlpmDbPtr
registerSyncdb h xs siglevel = liftIO $ do
    xs' <- newCString xs
    {#call alpm_register_syncdb #} h xs' $ makeBitmask siglevel

dbGetName :: MonadIO m => AlpmDbPtr -> m String
dbGetName db = liftIO $ do
    res <- {#call alpm_db_get_name #} db
    peekCString res

dbSetUsage :: MonadIO m => AlpmHandlePtr -> AlpmDbPtr -> [AlpmDbUsage] -> m ()
dbSetUsage h db xs = liftIO $
    throwAlpmErrorM' h (DbSetUsageError xs <$> dbGetName db) $
        {#call alpm_db_set_usage #} db (makeBitmask xs)

dbGetServers :: MonadIO m => AlpmDbPtr -> m [String]
dbGetServers db = liftIO $ do
    res <- {#call alpm_db_get_servers #} db
    peekAlpmStringList res

dbAddServer :: MonadIO m => AlpmHandlePtr -> AlpmDbPtr -> String -> m ()
dbAddServer h db xs = liftIO $ do
    xs' <- newCString xs
    throwAlpmErrorM' h (DbAddServerError xs <$> dbGetName db) $
        {#call alpm_db_add_server #} db xs'

dbSetServers :: MonadIO m => AlpmHandlePtr -> AlpmDbPtr -> [String] -> m ()
dbSetServers h db xss = liftIO $ do
    xss' <- mapM newCString xss
    xs <- toAlpmList xss'
    throwAlpmErrorM' h (DbSetServersError xss <$> dbGetName db) $
        {#call alpm_db_set_servers #} db xs

dbRemoveServer :: MonadIO m => AlpmHandlePtr -> AlpmDbPtr -> String -> m ()
dbRemoveServer h db xs = liftIO $ do
    xs' <- newCString xs
    throwAlpmErrorM' h (DbRemoveServerError xs <$> dbGetName db) $
        {#call alpm_db_remove_server #} db xs'

dbGetGroup :: MonadIO m => AlpmDbPtr -> String -> m AlpmGroup
dbGetGroup db xs = liftIO $ do
    xs' <- newCString xs
    {#call alpm_db_get_group #} db xs' >>= peek . castPtr

dbGetGroupcache :: MonadIO m => AlpmDbPtr -> m [AlpmGroup]
dbGetGroupcache db = liftIO $ do
    groups <- {#call alpm_db_get_groupcache #} db
    peekAlpmList groups

dbGetPkg :: MonadIO m => AlpmDbPtr -> String -> m AlpmPkgPtr
dbGetPkg h xs = liftIO $ do
    xs' <- newCString xs
    {#call alpm_db_get_pkg #} h xs'

dbGetPkgcache :: MonadIO m => AlpmDbPtr -> m [AlpmPkgPtr]
dbGetPkgcache db = liftIO $ do
    pkgs <- {#call alpm_db_get_pkgcache #} db
    map AlpmPkgPtr <$> fromAlpmList pkgs

dbSearch :: MonadIO m => AlpmHandlePtr -> AlpmDbPtr -> [String] -> m [AlpmPkgPtr]
dbSearch h db rxs = liftIO $
    withCStrings rxs $ \rxs' ->
    alloca $ \ret -> do
        needles <- toAlpmList rxs'
        throwAlpmErrorM' h (DbSearchError <$> dbGetName db <*> return rxs) $
            {#call alpm_db_search #} db needles ret
        ret' <- peek ret
        map AlpmPkgPtr <$> fromAlpmList ret'

dbUpdate :: MonadIO m => AlpmHandlePtr -> [AlpmDbPtr] -> Bool -> m UpdateResult
dbUpdate _ [] _ = return DbUpdateSkipped
dbUpdate h dbs doForce = liftIO $ do
    dbs' <- toAlpmList $ map unAlpmDbPtr dbs
    ec <- {#call alpm_db_update #} h dbs' (fromBool doForce)
    case ec of
        0 -> return DbUpdated
        1 -> return DbUpToDate
        _ -> do
            err <- errno h
            ns <- traverse dbGetName dbs
            throwM $ AlpmError err $ DbUpdateError ns
    where
        unAlpmDbPtr (AlpmDbPtr p) = p

findDbsSatisfier :: MonadIO m => AlpmHandlePtr -> [AlpmDbPtr] -> String -> m (Maybe AlpmPkgPtr)
findDbsSatisfier _ [] _ = return Nothing
findDbsSatisfier h dbs xs = liftIO $
    withAlpmList (map unAlpmDbPtr dbs) $ \dbs' ->
    withCString xs $ \xs' -> do
        res@(AlpmPkgPtr p) <- {#call alpm_find_dbs_satisfier #} h dbs' xs'
        return $ if p == nullPtr
            then Nothing
            else Just res
    where
        unAlpmDbPtr (AlpmDbPtr p) = p

findDbsSatisfier' :: MonadIO m => AlpmHandlePtr -> [AlpmDbPtr] -> AlpmDepend -> m (Maybe AlpmPkgPtr)
findDbsSatisfier' _ [] _ = return Nothing
findDbsSatisfier' h dbs x = liftIO $ alloca $ \p -> do
    poke p x
    xs <- depComputeString p
    --depFree p
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

data AlpmPkg = AlpmPkg
    { alpmPkgName :: AlpmPkgName
    , alpmPkgVersion :: AlpmVersion
    } deriving (Eq, Show)

instance Storable AlpmPkg where
    sizeOf _ = error "Storable.sizeOf not implemented for AlpmPkg !"
    alignment _ = error "Storable.alignment not implemented for AlpmPkg !"
    peek p = AlpmPkg
        <$> (pkgGetName    (AlpmPkgPtr $ castPtr p) >>= parseAlpmPkgName . T.pack)
        <*> (pkgGetVersion (AlpmPkgPtr $ castPtr p) >>= parseAlpmVersion . T.pack)
    poke _ _ = error "Storable.poke not implemented for AlpmPkg !"

pkgLoad :: MonadIO m => AlpmHandlePtr -> Bool -> [AlpmSiglevel] -> FilePath -> m AlpmPkgPtr
pkgLoad h full siglevel fp = liftIO $ do
    fp' <- newCString fp
    res <- malloc
    throwAlpmError' h (PkgLoadError fp) $
        {#call alpm_pkg_load #} h fp' (fromInteger $ fromBool full) (makeBitmask siglevel) res
    peek res

pkgFree :: MonadIO m => AlpmHandlePtr -> AlpmPkgPtr -> m ()
pkgFree h pkg = liftIO $
    throwAlpmErrorM' h (PkgFreeError <$> pkgGetName pkg) $
        {#call alpm_pkg_free #} pkg

pkgGetFilename :: MonadIO m => AlpmPkgPtr -> m FilePath
pkgGetFilename pkg = liftIO $ do
    res <- {#call alpm_pkg_get_filename #} pkg
    peekCString res

pkgGetOrigin :: MonadIO m => AlpmPkgPtr -> m AlpmPkgfrom
pkgGetOrigin = liftIO . fmap (toEnum . fromIntegral) . {#call alpm_pkg_get_origin #}

pkgGetDb :: MonadIO m => AlpmPkgPtr -> m (Maybe AlpmDbPtr)
pkgGetDb pkg = liftIO $ do
    res@(AlpmDbPtr p) <- {#call alpm_pkg_get_db #} pkg
    return $ if p == nullPtr
        then Nothing
        else Just res

pkgGetDepends :: MonadIO m => AlpmPkgPtr -> m [AlpmDepend]
pkgGetDepends pkg = liftIO $ do
    res <- {#call alpm_pkg_get_depends #} pkg
    peekAlpmList res

pkgGetName :: MonadIO m => AlpmPkgPtr -> m String
pkgGetName pkg = liftIO $ do
    res <- {#call alpm_pkg_get_name #} pkg
    peekCString res

pkgGetProvides :: MonadIO m => AlpmPkgPtr -> m [AlpmDepend]
pkgGetProvides pkg = liftIO $ do
    res <- {#call alpm_pkg_get_provides #} pkg
    peekAlpmList res

pkgGetVersion :: MonadIO m => AlpmPkgPtr -> m String
pkgGetVersion pkg = liftIO $ do
    res <- {#call alpm_pkg_get_version #} pkg
    peekCString res

pkgSetReason :: MonadIO m => AlpmHandlePtr -> AlpmPkgPtr -> AlpmPkgreason -> m ()
pkgSetReason h pkg x = liftIO $
    throwAlpmErrorM' h (PkgSetReasonError <$> pkgGetName pkg <*> return x) $
        {#call alpm_pkg_set_reason #} pkg $ fromIntegral $ fromEnum x

data AlpmPkgError
    = PkgFreeError String
    | PkgLoadError FilePath
    | PkgSetReasonError String AlpmPkgreason
    deriving (Eq, Show)

instance Exception AlpmPkgError

--------------------------------------------------------------------------------
-- Package names
--------------------------------------------------------------------------------

newtype AlpmPkgName = AlpmPkgName { unAlpmPkgName :: String }
    deriving (Eq, Ord, Show)

instance Display AlpmPkgName where
    display = display . T.pack . unAlpmPkgName

instance IsString AlpmPkgName where
    fromString = either (error . displayException) id . parseAlpmPkgName . T.pack

emptyAlpmPkgName :: AlpmPkgName
emptyAlpmPkgName = AlpmPkgName mempty

parseAlpmPkgName :: MonadThrow m => Text -> m AlpmPkgName
parseAlpmPkgName xs = case AT.parseOnly (pkgNameP <* AT.endOfInput) xs of
    Left e  -> throwM $ AlpmPkgNameParseException xs e
    Right x -> return x

pkgNameP :: AT.Parser AlpmPkgName
pkgNameP = do
    x  <- AT.satisfy (\c -> isAlphaNum c || AT.inClass "@_+" c)
    xs <- many (AT.satisfy (\c -> isAlphaNum c || AT.inClass "@._+-" c))
    return $ AlpmPkgName (x:xs)

data AlpmPkgNameParseException = AlpmPkgNameParseException Text String
    deriving (Eq, Show)

instance Exception AlpmPkgNameParseException

--------------------------------------------------------------------------------
-- Package versions
--------------------------------------------------------------------------------

data AlpmVersion = AlpmVersion
    { alpmVersionEpoch :: Maybe Natural
    , alpmVersionVer :: String
    , alpmVersionRel :: Natural
    , alpmVersionSubRel :: Maybe Natural
    } deriving Show

instance Eq AlpmVersion where
    x == y = compare x y == EQ

instance Ord AlpmVersion where
    x `compare` y = Unsafe.unsafePerformIO $ vercmp' x y

instance IsString AlpmVersion where
    fromString = either (error . displayException) id . parseAlpmVersion . T.pack

parseAlpmVersion :: MonadThrow m => Text -> m AlpmVersion
parseAlpmVersion xs = case AT.parseOnly (alpmVersionP <* AT.endOfInput) xs of
    Left e  -> throwM $ AlpmVersionParseException xs e
    Right x -> return x

alpmVersionP :: AT.Parser AlpmVersion
alpmVersionP = do
    mepoch <- AT.try $ optional $ AT.decimal <* AT.char ':'
    ver <- many $ AT.satisfy (AT.notInClass " :/-")
    void $ AT.char '-'
    rel <- AT.decimal
    msubrel <- optional $ do
        void $ AT.char '.'
        AT.decimal
    return AlpmVersion
        { alpmVersionEpoch  = mepoch
        , alpmVersionVer    = ver
        , alpmVersionRel    = rel
        , alpmVersionSubRel = msubrel
        }

showAlpmVersion :: AlpmVersion -> String
showAlpmVersion x = let
    epoch  = maybe "" ((++":") . show) $ alpmVersionEpoch x
    ver    = alpmVersionVer x
    rel    = show $ alpmVersionRel x
    subrel = maybe "" (("."++) . show) $ alpmVersionSubRel x
    in epoch ++ ver ++ "-" ++ rel ++ subrel

vercmp :: MonadIO m => String -> String -> m Ordering
vercmp xs ys = liftIO $
    withCString xs $ \xs' ->
    withCString ys $ \ys' -> do
        res <- {#call alpm_pkg_vercmp #} xs' ys'
        return $ res `compare` 0

vercmp' :: MonadIO m => AlpmVersion -> AlpmVersion -> m Ordering
vercmp' x y = vercmp (showAlpmVersion x) (showAlpmVersion y)

data AlpmVersionParseException = AlpmVersionParseException Text String
    deriving (Eq, Show)

instance Exception AlpmVersionParseException

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

data AlpmDepend = AlpmDepend
    { alpmDependName :: AlpmPkgName
    , alpmDependConstraint :: AlpmConstraint
    } deriving (Eq, Show)

instance Storable AlpmDepend where
    sizeOf _ = {#sizeof alpm_depend_t #}
    alignment _ = {#alignof alpm_depend_t #}
    peek p = AlpmDepend
        <$> ({#get alpm_depend_t->name #} p >>= peekCString >>= parseAlpmPkgName . T.pack)
        <*> peekConstraint p
    poke p x = do
        n <- newCString $ unAlpmPkgName $ alpmDependName x
        {#set alpm_depend_t->name #} p n
        pokeConstraint               p $ alpmDependConstraint x

depComputeString :: MonadIO m => AlpmDependPtr -> m String
depComputeString dep = liftIO $ {#call alpm_dep_compute_string #} dep >>= peekCString

depFromString :: MonadIO m => String -> m AlpmDependPtr
depFromString xs = liftIO $ withCString xs {#call alpm_dep_from_string #}

depFree :: MonadIO m => AlpmDependPtr -> m ()
depFree = liftIO . {#call alpm_dep_free #}

data AlpmConstraint
    = ConstraintAny
    | ConstraintLT AlpmVersion
    | ConstraintLE AlpmVersion
    | ConstraintEQ AlpmVersion
    | ConstraintGE AlpmVersion
    | ConstraintGT AlpmVersion
    deriving (Eq, Show)

constraintToDepmod :: AlpmConstraint -> AlpmDepmod
constraintToDepmod ConstraintAny  = AlpmDepModAny
constraintToDepmod ConstraintLT{} = AlpmDepModLt
constraintToDepmod ConstraintLE{} = AlpmDepModLe
constraintToDepmod ConstraintEQ{} = AlpmDepModEq
constraintToDepmod ConstraintGE{} = AlpmDepModGe
constraintToDepmod ConstraintGT{} = AlpmDepModGt

constraintVersion :: AlpmConstraint -> Maybe AlpmVersion
constraintVersion ConstraintAny    = Nothing
constraintVersion (ConstraintLT v) = Just v
constraintVersion (ConstraintLE v) = Just v
constraintVersion (ConstraintEQ v) = Just v
constraintVersion (ConstraintGE v) = Just v
constraintVersion (ConstraintGT v) = Just v

peekConstraint :: Ptr AlpmDepend -> IO AlpmConstraint
peekConstraint p = do
    mversion <- {#get alpm_depend_t->version #} p >>= peekMaybe (peekCString >=> parseAlpmVersion . T.pack)
    depmod <- toEnum . fromIntegral <$> {#get alpm_depend_t->mod #} p
    return $ case (mversion, depmod) of
        (Nothing, AlpmDepModAny) -> ConstraintAny
        (Just version, AlpmDepModEq) -> ConstraintEQ version
        (Just version, AlpmDepModLe) -> ConstraintLE version
        (Just version, AlpmDepModGe) -> ConstraintGE version
        (Just version, AlpmDepModLt) -> ConstraintLT version
        (Just version, AlpmDepModGt) -> ConstraintGT version
        _ -> error $ "Illegal version/depmod compination: " <> show mversion <> " " <> show depmod

pokeConstraint :: Ptr AlpmDepend -> AlpmConstraint -> IO ()
pokeConstraint p x = do
    v <- case constraintVersion x of
        Nothing -> return nullPtr
        Just v  -> newCString $ showAlpmVersion v
    let depmod = constraintToDepmod x
    {#set alpm_depend_t->version #} p v
    {#set alpm_depend_t->mod #}     p $ fromIntegral $ fromEnum depmod

--------------------------------------------------------------------------------
-- Groups
--------------------------------------------------------------------------------

data AlpmGroup = AlpmGroup
    { alpmGroupName :: String
    , alpmGroupPackages :: [AlpmPkgPtr]
    } deriving Show

instance Storable AlpmGroup where
    sizeOf _ = {#sizeof alpm_group_t #}
    alignment _ = {#alignof alpm_group_t #}
    peek p = AlpmGroup
        <$> ({#get alpm_group_t->name #}     p >>= peekCString )
        <*> ({#get alpm_group_t->packages #} p >>= fmap (map AlpmPkgPtr) . fromAlpmList)
    poke _ _ = error "Storable.poke not implemented for AlpmGroup !"

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

withTrans :: MonadUnliftIO m => AlpmHandlePtr -> [AlpmTransflag] -> m a -> m a
withTrans h flags f = bracket (transInit h flags) (\_ -> transRelease h) (\_ -> f)

transInit :: MonadIO m => AlpmHandlePtr -> [AlpmTransflag] -> m ()
transInit h flags = liftIO $
    throwAlpmError h $
        {#call alpm_trans_init #} h $ makeBitmask flags

transPrepare :: MonadIO m => AlpmHandlePtr -> m ()
transPrepare h = liftIO $
    alloca $ \p -> do
        let onErr :: AlpmErrNo -> AlpmUnknownError -> IO AlpmTransactionError
            onErr err _ = do
                p' <- peek p
                case err of
                    AlpmErrPkgInvalidArch -> TransPrepareTextError
                        <$> peekAlpmStringListFree p'
                    AlpmErrUnsatisfiedDeps -> TransPrepareDepmissingError
                        <$> peekAlpmListFree ({#call alpm_depmissing_free #} . castPtr) p'
                    AlpmErrConflictingDeps -> TransPrepareConflictError
                        <$> peekAlpmListFree ({#call alpm_conflict_free #}   . castPtr) p'
                    _ -> return TransPrepareOtherError
        modifyAlpmErrorM onErr $ throwAlpmError h $ {#call alpm_trans_prepare #} h p
        p' <- peek p
        {#call alpm_list_free #} p'

transCommit :: MonadIO m => AlpmHandlePtr -> m ()
transCommit h = liftIO $
    alloca $ \p -> do
        let onErr :: AlpmErrNo -> AlpmUnknownError -> IO AlpmTransactionError
            onErr err _ = do
                p' <- peek p
                case err of
                    AlpmErrFileConflicts -> TransCommitFileconflictError
                        <$> peekAlpmListFree ({#call alpm_fileconflict_free #} . castPtr) p'
                    AlpmErrPkgInvalid -> TransCommitTextError
                        <$> peekAlpmStringListFree p'
                    AlpmErrPkgInvalidChecksum -> TransCommitTextError
                        <$> peekAlpmStringListFree p'
                    AlpmErrPkgInvalidSig -> TransCommitTextError
                        <$> peekAlpmStringListFree p'
                    _ -> return TransCommitOtherError
        modifyAlpmErrorM onErr $ throwAlpmError h $ {#call alpm_trans_commit #} h p

transInterrupt :: MonadIO m => AlpmHandlePtr -> m ()
transInterrupt h = liftIO $ throwAlpmError h $ {#call alpm_trans_interrupt #} h

transRelease :: MonadIO m => AlpmHandlePtr -> m ()
transRelease h = liftIO $ throwAlpmError h $ {#call alpm_trans_release #} h

transGetAdd :: MonadIO m => AlpmHandlePtr -> m [AlpmPkgPtr]
transGetAdd h = liftIO $ do
    res <- {#call alpm_trans_get_add #} h
    map AlpmPkgPtr <$> fromAlpmList res

transGetRemove :: MonadIO m => AlpmHandlePtr -> m [AlpmPkgPtr]
transGetRemove h = liftIO $ do
    res <- {#call alpm_trans_get_remove #} h
    map AlpmPkgPtr <$> fromAlpmList res

addPkg :: MonadIO m => AlpmHandlePtr -> AlpmPkgPtr -> m ()
addPkg h pkg = liftIO $
    throwAlpmErrorM' h (AddPkgError <$> pkgGetName pkg) $
        {#call alpm_add_pkg #} h pkg

removePkg :: MonadIO m => AlpmHandlePtr -> AlpmPkgPtr -> m ()
removePkg h pkg = liftIO $
    throwAlpmErrorM' h (RemovePkgError <$> pkgGetName pkg) $
        {#call alpm_remove_pkg #} h pkg

data AlpmTransactionError
    = AddPkgError String
    | RemovePkgError String
    | TransCommitFileconflictError [AlpmFileconflict]
    | TransCommitOtherError
    | TransCommitTextError [String]
    | TransPrepareConflictError [AlpmConflict]
    | TransPrepareDepmissingError [AlpmDepmissing]
    | TransPrepareOtherError
    | TransPrepareTextError [String]
    deriving (Eq, Show)

instance Exception AlpmTransactionError

data AlpmConflict = AlpmConflict
    { alpmConflictPkg1   :: AlpmPkg
    , alpmConflictPkg2   :: AlpmPkg
    , alpmConflictReason :: AlpmDepend
    } deriving (Eq, Show)

instance Storable AlpmConflict where
    sizeOf _ = {#sizeof alpm_conflict_t #}
    alignment _ = {#alignof alpm_conflict_t #}
    peek p = do
        alpmConflictPkg1 <- do
            AlpmPkgPtr ptr <- {#get alpm_conflict_t->package1 #} p
            peek (castPtr ptr)
        alpmConflictPkg2 <- do
            AlpmPkgPtr ptr <- {#get alpm_conflict_t->package2 #} p
            peek (castPtr ptr)
        alpmConflictReason <- {#get alpm_conflict_t->reason #} p >>= peek . castPtr
        return $ AlpmConflict {..}
    poke _ _ = error "Storable.poke not implemented for AlpmConflict !"

data AlpmDepmissing = AlpmDepmissing
    { alpmDepmissingTarget     :: String
    , alpmDepmissingDepend     :: AlpmDepend
    , alpmDepmissingCausingPkg :: Maybe String
    } deriving (Eq, Show)

instance Storable AlpmDepmissing where
    sizeOf _ = {#sizeof alpm_depmissing_t #}
    alignment _ = {#alignof alpm_depmissing_t #}
    peek p = AlpmDepmissing
        <$> ({#get alpm_depmissing_t->target #}     p >>= peekCString)
        <*> ({#get alpm_depmissing_t->depend #}     p >>= peek . castPtr)
        <*> ({#get alpm_depmissing_t->causingpkg #} p >>= peekMaybe peekCString)
    poke _ _ = error "Storable.poke not implemented for AlpmDepmissing !"

data AlpmFileconflict = AlpmFileconflict
    { alpmFileconflictTarget :: String
    , alpmFileconflictType :: AlpmFileconflicttype
    , alpmFileconflictFile :: FilePath
    , alpmFileconflictCtarget :: String
    } deriving (Eq, Show)

instance Storable AlpmFileconflict where
    sizeOf _ = {#sizeof alpm_fileconflict_t #}
    alignment _ = {#alignof alpm_fileconflict_t #}
    peek p = AlpmFileconflict
        <$> (                          {#get alpm_fileconflict_t->target #}  p >>= peekCString)
        <*> (toEnum . fromIntegral <$> {#get alpm_fileconflict_t->type #}    p)
        <*> (                          {#get alpm_fileconflict_t->file #}    p >>= peekCString)
        <*> (                          {#get alpm_fileconflict_t->ctarget #} p >>= peekCString)
    poke _ _ = error "Storable.poke not implemented for AlpmFileconflict !"

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

errno :: MonadIO m => AlpmHandlePtr -> m AlpmErrNo
errno h = toEnum . fromIntegral <$> liftIO ({#call alpm_errno #} h)

strerror :: MonadIO m => AlpmErrNo -> m String
strerror e = liftIO $ do
    xs <- {#call alpm_strerror #} $ fromIntegral $ fromEnum e
    peekCString xs

throwAlpmError :: (MonadIO m, MonadThrow m) => AlpmHandlePtr -> m CInt -> m ()
throwAlpmError h = throwAlpmError' h UnknownAlpmError

throwAlpmError' :: (MonadIO m, MonadThrow m, Show e, Typeable e) => AlpmHandlePtr -> e -> m CInt -> m ()
throwAlpmError' h x = throwAlpmErrorM' h (return x)

throwAlpmErrorM' :: (MonadIO m, MonadThrow m, Show e, Typeable e) => AlpmHandlePtr -> m e -> m CInt -> m ()
throwAlpmErrorM' h x k = do
    ec <- k
    unless (ec == 0) $
        throwM =<< (AlpmError <$> errno h <*> x)

modifyAlpmErrorM :: (MonadUnliftIO m, MonadThrow m, Show e, Show e', Typeable e, Typeable e') => (AlpmErrNo -> e -> m e') -> m c -> m c
modifyAlpmErrorM f = handle (\(AlpmError err x) -> f err x >>= throwM . AlpmError err)

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

fromAlpmList :: AlpmListPtr -> IO [Ptr a]
fromAlpmList = traverseAlpmList (return . (:[]))

peekAlpmList :: Storable a => AlpmListPtr -> IO [a]
peekAlpmList = traverseAlpmList $ \p' -> do
    x <- peek p'
    return [x]

peekAlpmListFree :: Storable a => (Ptr a -> IO ()) -> AlpmListPtr -> IO [a]
peekAlpmListFree freeF p = freeListAfter p $
    traverseAlpmList $ \p' -> do
        x <- peek p'
        freeF p'
        return [x]

peekAlpmStringList :: AlpmListPtr -> IO [String]
peekAlpmStringList = traverseAlpmList $ \p' -> do
    x <- peekCString p'
    return [x]

peekAlpmStringListFree :: AlpmListPtr -> IO [String]
peekAlpmStringListFree p = do
    res <- traverseAlpmList (\p' -> do
        x <- peekCString p'
        free p'
        return [x]
        ) p
    {#call alpm_list_free #} p
    return res

toAlpmList :: [Ptr a] -> IO AlpmListPtr
toAlpmList = foldM (\memo -> {#call alpm_list_add #} memo . castPtr) nullPtr

withAlpmList :: [Ptr a] -> (AlpmListPtr -> IO b) -> IO b
withAlpmList xs = bracket (toAlpmList xs) {#call alpm_list_free #}

freeListAfter :: AlpmListPtr -> (AlpmListPtr -> IO a) -> IO a
freeListAfter p k = do
    res <- k p
    {#call alpm_list_free #} p
    return res

traverseAlpmList :: Monoid m => (Ptr a -> IO m) -> AlpmListPtr -> IO m
traverseAlpmList f = go id
    where
        go !contF xs
            | xs == nullPtr = return $ contF mempty
            | otherwise = do
                x <- {#get alpm_list_t->data #} xs
                xs' <- {#get alpm_list_t->next #} xs
                y <- f $ castPtr x
                go (contF . (y <>)) xs'

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings xss f = go [] xss
    where
        go !acc []     = f $ reverse acc
        go !acc (x:xs) = withCString x (\x' -> go (x':acc) xs)

peekMaybe :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
peekMaybe f p
    | p == nullPtr = return Nothing
    | otherwise    = Just <$> f p

pokeMaybe :: (MonadIO m, Storable a) => (Ptr a -> a -> m ()) -> Maybe a -> m (Ptr a)
pokeMaybe _ Nothing  = return nullPtr
pokeMaybe f (Just x) = do
    p <- liftIO malloc
    f p x
    return p

makeBitmask :: Enum a => [a] -> CInt
makeBitmask = foldl' (\memo x -> memo .|. fromIntegral (fromEnum x)) zeroBits
