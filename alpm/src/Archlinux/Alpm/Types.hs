{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Archlinux.Alpm.Types (
    AlpmPkg (..),
    AlpmConstraint (..),
    AlpmDepend (..),
    AlpmConflict (..),
    peekAlpmConflict,
    AlpmDepmissing (..),
    peekAlpmDepmissing,
    AlpmFileconflict (..),
    peekAlpmFileconflict,
    AlpmGroup (..),
    peekAlpmGroup,
    AlpmEvent (..),
    peekAlpmEvent,
    AlpmEventFor (..),
    AlpmQuestion (..),
    peekAlpmQuestion,
    AlpmQuestionFor (..),
    vercmp,
    vercmp',
) where

import Control.Exception (Exception, throwIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Text.Display (Display (..))
import Data.Text.Short (ShortText)
import Foreign hiding (void)
import Foreign.C
import Language.Haskell.TH.Syntax (Lift)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Functions ()
import Prelude

import Archlinux.Alpm.Binding as Binding
import Archlinux.Alpm.Package.Types hiding (name, version)
import Archlinux.Alpm.Utils

import ShortText.Extra qualified as ShortText

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

newtype DatabaseName = DatabaseName {unDatabaseName :: ShortText}
    deriving stock (Eq, Lift, Ord, Show)
    deriving (Display) via ShortText

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data AlpmEvent
    = CheckdepsStartEvent (AlpmEventFor 'AlpmEventCheckdepsStart)
    | CheckdepsDoneEvent (AlpmEventFor 'AlpmEventCheckdepsDone)
    | FileconflictsStartEvent (AlpmEventFor 'AlpmEventFileconflictsStart)
    | FileconflictsDoneEvent (AlpmEventFor 'AlpmEventFileconflictsDone)
    | ResolvedepsStartEvent (AlpmEventFor 'AlpmEventResolvedepsStart)
    | ResolvedepsDoneEvent (AlpmEventFor 'AlpmEventResolvedepsDone)
    | InterconflictsStartEvent (AlpmEventFor 'AlpmEventInterconflictsStart)
    | InterconflictsDoneEvent (AlpmEventFor 'AlpmEventInterconflictsDone)
    | TransactionStartEvent (AlpmEventFor 'AlpmEventTransactionStart)
    | TransactionDoneEvent (AlpmEventFor 'AlpmEventTransactionDone)
    | PackageOperationStartEvent (AlpmEventFor 'AlpmEventPackageOperationStart)
    | PackageOperationDoneEvent (AlpmEventFor 'AlpmEventPackageOperationDone)
    | IntegrityStartEvent (AlpmEventFor 'AlpmEventIntegrityStart)
    | IntegrityDoneEvent (AlpmEventFor 'AlpmEventIntegrityDone)
    | LoadStartEvent (AlpmEventFor 'AlpmEventLoadStart)
    | LoadDoneEvent (AlpmEventFor 'AlpmEventLoadDone)
    | ScriptletInfoEvent (AlpmEventFor 'AlpmEventScriptletInfo)
    | DbRetrieveStartEvent (AlpmEventFor 'AlpmEventDbRetrieveStart)
    | DbRetrieveDoneEvent (AlpmEventFor 'AlpmEventDbRetrieveDone)
    | DbRetrieveFailedEvent (AlpmEventFor 'AlpmEventDbRetrieveFailed)
    | PkgRetrieveStartEvent (AlpmEventFor 'AlpmEventPkgRetrieveStart)
    | PkgRetrieveDoneEvent (AlpmEventFor 'AlpmEventPkgRetrieveDone)
    | PkgRetrieveFailedEvent (AlpmEventFor 'AlpmEventPkgRetrieveFailed)
    | DiskspaceStartEvent (AlpmEventFor 'AlpmEventDiskspaceStart)
    | DiskspaceDoneEvent (AlpmEventFor 'AlpmEventDiskspaceDone)
    | OptdepRemovalEvent (AlpmEventFor 'AlpmEventOptdepRemoval)
    | DatabaseMissingEvent (AlpmEventFor 'AlpmEventDatabaseMissing)
    | KeyringStartEvent (AlpmEventFor 'AlpmEventKeyringStart)
    | KeyringDoneEvent (AlpmEventFor 'AlpmEventKeyringDone)
    | KeyDownloadStartEvent (AlpmEventFor 'AlpmEventKeyDownloadStart)
    | KeyDownloadDoneEvent (AlpmEventFor 'AlpmEventKeyDownloadDone)
    | PacnewCreatedEvent (AlpmEventFor 'AlpmEventPacnewCreated)
    | PacsaveCreatedEvent (AlpmEventFor 'AlpmEventPacsaveCreated)
    | HookStartEvent (AlpmEventFor 'AlpmEventHookStart)
    | HookDoneEvent (AlpmEventFor 'AlpmEventHookDone)
    | HookRunStartEvent (AlpmEventFor 'AlpmEventHookRunStart)
    | HookRunDoneEvent (AlpmEventFor 'AlpmEventHookRunDone)
    deriving (Show)

instance Display AlpmEvent where
    displayBuilder = displayBuilder . show -- TODO

peekAlpmEvent :: Ptr AlpmEvent -> IO AlpmEvent
peekAlpmEvent p = do
    t <- toEnum . fromIntegral <$> get__event_t__type p
    case t of
        AlpmEventCheckdepsStart -> CheckdepsStartEvent <$> peekCheckdepsStartEvent (castPtr p)
        AlpmEventCheckdepsDone -> CheckdepsDoneEvent <$> peekCheckdepsDoneEvent (castPtr p)
        AlpmEventFileconflictsStart -> FileconflictsStartEvent <$> peekFileconflictsStartEvent (castPtr p)
        AlpmEventFileconflictsDone -> FileconflictsDoneEvent <$> peekFileconflictsDoneEvent (castPtr p)
        AlpmEventResolvedepsStart -> ResolvedepsStartEvent <$> peekResolvedepsStartEvent (castPtr p)
        AlpmEventResolvedepsDone -> ResolvedepsDoneEvent <$> peekResolvedepsDoneEvent (castPtr p)
        AlpmEventInterconflictsStart -> InterconflictsStartEvent <$> peekInterconflictsStartEvent (castPtr p)
        AlpmEventInterconflictsDone -> InterconflictsDoneEvent <$> peekInterconflictsDoneEvent (castPtr p)
        AlpmEventTransactionStart -> TransactionStartEvent <$> peekTransactionStartEvent (castPtr p)
        AlpmEventTransactionDone -> TransactionDoneEvent <$> peekTransactionDoneEvent (castPtr p)
        AlpmEventPackageOperationStart -> PackageOperationStartEvent <$> peekPackageOperationStartEvent (castPtr p)
        AlpmEventPackageOperationDone -> PackageOperationDoneEvent <$> peekPackageOperationDoneEvent (castPtr p)
        AlpmEventIntegrityStart -> IntegrityStartEvent <$> peekIntegrityStartEvent (castPtr p)
        AlpmEventIntegrityDone -> IntegrityDoneEvent <$> peekIntegrityDoneEvent (castPtr p)
        AlpmEventLoadStart -> LoadStartEvent <$> peekLoadStartEvent (castPtr p)
        AlpmEventLoadDone -> LoadDoneEvent <$> peekLoadDoneEvent (castPtr p)
        AlpmEventScriptletInfo -> ScriptletInfoEvent <$> peekScriptletInfoEvent (castPtr p)
        AlpmEventDbRetrieveStart -> DbRetrieveStartEvent <$> peekDbRetrieveStartEvent (castPtr p)
        AlpmEventDbRetrieveDone -> DbRetrieveDoneEvent <$> peekDbRetrieveDoneEvent (castPtr p)
        AlpmEventDbRetrieveFailed -> DbRetrieveFailedEvent <$> peekDbRetrieveFailedEvent (castPtr p)
        AlpmEventPkgRetrieveStart -> PkgRetrieveStartEvent <$> peekPkgRetrieveStartEvent (castPtr p)
        AlpmEventPkgRetrieveDone -> PkgRetrieveDoneEvent <$> peekPkgRetrieveDoneEvent (castPtr p)
        AlpmEventPkgRetrieveFailed -> PkgRetrieveFailedEvent <$> peekPkgRetrieveFailedEvent (castPtr p)
        AlpmEventDiskspaceStart -> DiskspaceStartEvent <$> peekDiskspaceStartEvent (castPtr p)
        AlpmEventDiskspaceDone -> DiskspaceDoneEvent <$> peekDiskspaceDoneEvent (castPtr p)
        AlpmEventOptdepRemoval -> OptdepRemovalEvent <$> peekOptdepRemovalEvent (castPtr p)
        AlpmEventDatabaseMissing -> DatabaseMissingEvent <$> peekDatabaseMissingEvent (castPtr p)
        AlpmEventKeyringStart -> KeyringStartEvent <$> peekKeyringStartEvent (castPtr p)
        AlpmEventKeyringDone -> KeyringDoneEvent <$> peekKeyringDoneEvent (castPtr p)
        AlpmEventKeyDownloadStart -> KeyDownloadStartEvent <$> peekKeyDownloadStartEvent (castPtr p)
        AlpmEventKeyDownloadDone -> KeyDownloadDoneEvent <$> peekKeyDownloadDoneEvent (castPtr p)
        AlpmEventPacnewCreated -> PacnewCreatedEvent <$> peekPacnewCreatedEvent (castPtr p)
        AlpmEventPacsaveCreated -> PacsaveCreatedEvent <$> peekPacsaveCreatedEvent (castPtr p)
        AlpmEventHookStart -> HookStartEvent <$> peekHookStartEvent (castPtr p)
        AlpmEventHookDone -> HookDoneEvent <$> peekHookDoneEvent (castPtr p)
        AlpmEventHookRunStart -> HookRunStartEvent <$> peekHookRunStartEvent (castPtr p)
        AlpmEventHookRunDone -> HookRunDoneEvent <$> peekHookRunDoneEvent (castPtr p)

data instance AlpmEventFor 'AlpmEventCheckdepsStart = CheckdepsStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventCheckdepsDone = CheckdepsDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventFileconflictsStart = FileconflictsStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventFileconflictsDone = FileconflictsDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventResolvedepsStart = ResolvedepsStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventResolvedepsDone = ResolvedepsDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventInterconflictsStart = InterconflictsStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventInterconflictsDone = InterconflictsDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventTransactionStart = TransactionStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventTransactionDone = TransactionDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventPackageOperationStart = PackageOperationStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventPackageOperationDone = PackageOperationDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventIntegrityStart = IntegrityStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventIntegrityDone = IntegrityDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventLoadStart = LoadStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventLoadDone = LoadDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventScriptletInfo = ScriptletInfo
    deriving (Show)

data instance AlpmEventFor 'AlpmEventDbRetrieveStart = DbRetrieveStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventDbRetrieveDone = DbRetrieveDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventDbRetrieveFailed = DbRetrieveFailed
    deriving (Show)

data instance AlpmEventFor 'AlpmEventPkgRetrieveStart = PkgRetrieveStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventPkgRetrieveDone = PkgRetrieveDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventPkgRetrieveFailed = PkgRetrieveFailed
    deriving (Show)

data instance AlpmEventFor 'AlpmEventDiskspaceStart = DiskspaceStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventDiskspaceDone = DiskspaceDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventOptdepRemoval = OptdepRemoval
    deriving (Show)

data instance AlpmEventFor 'AlpmEventDatabaseMissing = DatabaseMissing
    { databaseMissingName :: DatabaseName
    }
    deriving (Show)

data instance AlpmEventFor 'AlpmEventKeyringStart = KeyringStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventKeyringDone = KeyringDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventKeyDownloadStart = KeyDownloadStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventKeyDownloadDone = KeyDownloadDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventPacnewCreated = PacnewCreated
    deriving (Show)

data instance AlpmEventFor 'AlpmEventPacsaveCreated = PacsaveCreated
    deriving (Show)

data instance AlpmEventFor 'AlpmEventHookStart = HookStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventHookDone = HookDone
    deriving (Show)

data instance AlpmEventFor 'AlpmEventHookRunStart = HookRunStart
    deriving (Show)

data instance AlpmEventFor 'AlpmEventHookRunDone = HookRunDone
    deriving (Show)

peekCheckdepsStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventCheckdepsStart)
    -> IO (AlpmEventFor 'AlpmEventCheckdepsStart)
peekCheckdepsStartEvent _p = pure CheckdepsStart

peekCheckdepsDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventCheckdepsDone)
    -> IO (AlpmEventFor 'AlpmEventCheckdepsDone)
peekCheckdepsDoneEvent _p = pure CheckdepsDone

peekFileconflictsStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventFileconflictsStart)
    -> IO (AlpmEventFor 'AlpmEventFileconflictsStart)
peekFileconflictsStartEvent _p = pure FileconflictsStart

peekFileconflictsDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventFileconflictsDone)
    -> IO (AlpmEventFor 'AlpmEventFileconflictsDone)
peekFileconflictsDoneEvent _p = pure FileconflictsDone

peekResolvedepsStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventResolvedepsStart)
    -> IO (AlpmEventFor 'AlpmEventResolvedepsStart)
peekResolvedepsStartEvent _p = pure ResolvedepsStart

peekResolvedepsDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventResolvedepsDone)
    -> IO (AlpmEventFor 'AlpmEventResolvedepsDone)
peekResolvedepsDoneEvent _p = pure ResolvedepsDone

peekInterconflictsStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventInterconflictsStart)
    -> IO (AlpmEventFor 'AlpmEventInterconflictsStart)
peekInterconflictsStartEvent _p = pure InterconflictsStart

peekInterconflictsDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventInterconflictsDone)
    -> IO (AlpmEventFor 'AlpmEventInterconflictsDone)
peekInterconflictsDoneEvent _p = pure InterconflictsDone

peekTransactionStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventTransactionStart)
    -> IO (AlpmEventFor 'AlpmEventTransactionStart)
peekTransactionStartEvent _p = pure TransactionStart

peekTransactionDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventTransactionDone)
    -> IO (AlpmEventFor 'AlpmEventTransactionDone)
peekTransactionDoneEvent _p = pure TransactionDone

peekPackageOperationStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventPackageOperationStart)
    -> IO (AlpmEventFor 'AlpmEventPackageOperationStart)
peekPackageOperationStartEvent _p = pure PackageOperationStart

peekPackageOperationDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventPackageOperationDone)
    -> IO (AlpmEventFor 'AlpmEventPackageOperationDone)
peekPackageOperationDoneEvent _p = pure PackageOperationDone

peekIntegrityStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventIntegrityStart)
    -> IO (AlpmEventFor 'AlpmEventIntegrityStart)
peekIntegrityStartEvent _p = pure IntegrityStart

peekIntegrityDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventIntegrityDone)
    -> IO (AlpmEventFor 'AlpmEventIntegrityDone)
peekIntegrityDoneEvent _p = pure IntegrityDone

peekLoadStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventLoadStart)
    -> IO (AlpmEventFor 'AlpmEventLoadStart)
peekLoadStartEvent _p = pure LoadStart

peekLoadDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventLoadDone)
    -> IO (AlpmEventFor 'AlpmEventLoadDone)
peekLoadDoneEvent _p = pure LoadDone

peekScriptletInfoEvent
    :: Ptr (AlpmEventFor 'AlpmEventScriptletInfo)
    -> IO (AlpmEventFor 'AlpmEventScriptletInfo)
peekScriptletInfoEvent _p = pure ScriptletInfo

peekDbRetrieveStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventDbRetrieveStart)
    -> IO (AlpmEventFor 'AlpmEventDbRetrieveStart)
peekDbRetrieveStartEvent _p = pure DbRetrieveStart

peekDbRetrieveDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventDbRetrieveDone)
    -> IO (AlpmEventFor 'AlpmEventDbRetrieveDone)
peekDbRetrieveDoneEvent _p = pure DbRetrieveDone

peekDbRetrieveFailedEvent
    :: Ptr (AlpmEventFor 'AlpmEventDbRetrieveFailed)
    -> IO (AlpmEventFor 'AlpmEventDbRetrieveFailed)
peekDbRetrieveFailedEvent _p = pure DbRetrieveFailed

peekPkgRetrieveStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventPkgRetrieveStart)
    -> IO (AlpmEventFor 'AlpmEventPkgRetrieveStart)
peekPkgRetrieveStartEvent _p = pure PkgRetrieveStart

peekPkgRetrieveDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventPkgRetrieveDone)
    -> IO (AlpmEventFor 'AlpmEventPkgRetrieveDone)
peekPkgRetrieveDoneEvent _p = pure PkgRetrieveDone

peekPkgRetrieveFailedEvent
    :: Ptr (AlpmEventFor 'AlpmEventPkgRetrieveFailed)
    -> IO (AlpmEventFor 'AlpmEventPkgRetrieveFailed)
peekPkgRetrieveFailedEvent _p = pure PkgRetrieveFailed

peekDiskspaceStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventDiskspaceStart)
    -> IO (AlpmEventFor 'AlpmEventDiskspaceStart)
peekDiskspaceStartEvent _p = pure DiskspaceStart

peekDiskspaceDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventDiskspaceDone)
    -> IO (AlpmEventFor 'AlpmEventDiskspaceDone)
peekDiskspaceDoneEvent _p = pure DiskspaceDone

peekOptdepRemovalEvent
    :: Ptr (AlpmEventFor 'AlpmEventOptdepRemoval)
    -> IO (AlpmEventFor 'AlpmEventOptdepRemoval)
peekOptdepRemovalEvent _p = pure OptdepRemoval

peekDatabaseMissingEvent
    :: Ptr (AlpmEventFor 'AlpmEventDatabaseMissing)
    -> IO (AlpmEventFor 'AlpmEventDatabaseMissing)
peekDatabaseMissingEvent p = do
    let
        p' :: AlpmEventDatabaseMissingPtr
        p' = castPtr p
    DatabaseMissing
        <$> ( DatabaseName
                <$> (ShortText.unsafePackCString =<< get__event_database_missing_t__dbname p')
            )

peekKeyringStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventKeyringStart)
    -> IO (AlpmEventFor 'AlpmEventKeyringStart)
peekKeyringStartEvent _p = pure KeyringStart

peekKeyringDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventKeyringDone)
    -> IO (AlpmEventFor 'AlpmEventKeyringDone)
peekKeyringDoneEvent _p = pure KeyringDone

peekKeyDownloadStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventKeyDownloadStart)
    -> IO (AlpmEventFor 'AlpmEventKeyDownloadStart)
peekKeyDownloadStartEvent _p = pure KeyDownloadStart

peekKeyDownloadDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventKeyDownloadDone)
    -> IO (AlpmEventFor 'AlpmEventKeyDownloadDone)
peekKeyDownloadDoneEvent _p = pure KeyDownloadDone

peekPacnewCreatedEvent
    :: Ptr (AlpmEventFor 'AlpmEventPacnewCreated)
    -> IO (AlpmEventFor 'AlpmEventPacnewCreated)
peekPacnewCreatedEvent _p = pure PacnewCreated

peekPacsaveCreatedEvent
    :: Ptr (AlpmEventFor 'AlpmEventPacsaveCreated)
    -> IO (AlpmEventFor 'AlpmEventPacsaveCreated)
peekPacsaveCreatedEvent _p = pure PacsaveCreated

peekHookStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventHookStart)
    -> IO (AlpmEventFor 'AlpmEventHookStart)
peekHookStartEvent _p = pure HookStart

peekHookDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventHookDone)
    -> IO (AlpmEventFor 'AlpmEventHookDone)
peekHookDoneEvent _p = pure HookDone

peekHookRunStartEvent
    :: Ptr (AlpmEventFor 'AlpmEventHookRunStart)
    -> IO (AlpmEventFor 'AlpmEventHookRunStart)
peekHookRunStartEvent _p = pure HookRunStart

peekHookRunDoneEvent
    :: Ptr (AlpmEventFor 'AlpmEventHookRunDone)
    -> IO (AlpmEventFor 'AlpmEventHookRunDone)
peekHookRunDoneEvent _p = pure HookRunDone

--------------------------------------------------------------------------------
-- Questions
--------------------------------------------------------------------------------

data AlpmQuestion
    = InstallIgnorepkgQuestion (AlpmQuestionFor 'AlpmQuestionInstallIgnorepkg)
    | ReplacePkgQuestion (AlpmQuestionFor 'AlpmQuestionReplacePkg)
    | ConflictPkgQuestion (AlpmQuestionFor 'AlpmQuestionConflictPkg)
    | CorruptedPkgQuestion (AlpmQuestionFor 'AlpmQuestionCorruptedPkg)
    | RemovePkgsQuestion (AlpmQuestionFor 'AlpmQuestionRemovePkgs)
    | SelectProviderQuestion (AlpmQuestionFor 'AlpmQuestionSelectProvider)
    | ImportKeyQuestion (AlpmQuestionFor 'AlpmQuestionImportKey)
    deriving (Show)

peekAlpmQuestion :: Ptr AlpmQuestion -> IO AlpmQuestion
peekAlpmQuestion p = do
    t <- toEnum . fromIntegral <$> get__question_t__type p
    case t of
        AlpmQuestionInstallIgnorepkg -> InstallIgnorepkgQuestion <$> peekInstallIgnorepkgQuestion (castPtr p)
        AlpmQuestionReplacePkg -> ReplacePkgQuestion <$> peekReplacePkgQuestion (castPtr p)
        AlpmQuestionConflictPkg -> ConflictPkgQuestion <$> peekConflictPkgQuestion (castPtr p)
        AlpmQuestionCorruptedPkg -> CorruptedPkgQuestion <$> peekCorruptedPkgQuestion (castPtr p)
        AlpmQuestionRemovePkgs -> RemovePkgsQuestion <$> peekRemovePkgsQuestion (castPtr p)
        AlpmQuestionSelectProvider -> SelectProviderQuestion <$> peekSelectProviderQuestion (castPtr p)
        AlpmQuestionImportKey -> ImportKeyQuestion <$> peekImportKeyQuestion (castPtr p)

data instance AlpmQuestionFor 'AlpmQuestionInstallIgnorepkg = InstallIgnorepkg
    { installIgnorepkgAnswer :: Int -> IO ()
    , installIgnorepkgPkg :: AlpmPkgPtr
    }
    deriving (Show)

data instance AlpmQuestionFor 'AlpmQuestionReplacePkg = ReplacePkg
    { replacePkgAnswer :: Int -> IO ()
    , replacePkgOldpkg :: AlpmPkgPtr
    , replacePkgNewpkg :: AlpmPkgPtr
    , replacePkgNewdb :: AlpmDbPtr
    }
    deriving (Show)

data instance AlpmQuestionFor 'AlpmQuestionConflictPkg = ConflictPkg
    { conflictPkgAnswer :: Int -> IO ()
    , conflictPkgConflict :: AlpmConflictPtr
    }
    deriving (Show)

data instance AlpmQuestionFor 'AlpmQuestionCorruptedPkg = CorruptedPkg
    { corruptedPkgAnswer :: Int -> IO ()
    , corruptedPkgFilepath :: CString
    , corruptedPkgReason :: AlpmErrNo -- TODO: Should we set this?
    }
    deriving (Show)

data instance AlpmQuestionFor 'AlpmQuestionRemovePkgs = RemovePkgs
    { removePkgsAnswer :: Int -> IO ()
    , removePkgsPackages :: [AlpmPkgPtr]
    }
    deriving (Show)

data instance AlpmQuestionFor 'AlpmQuestionSelectProvider = SelectProvider
    { selectProviderAnswer :: Int -> IO ()
    , selectProviderDepend :: AlpmDependPtr
    , selectProviderProviders :: [AlpmPkgPtr]
    }
    deriving (Show)

data instance AlpmQuestionFor 'AlpmQuestionImportKey = ImportKey
    { importKeyImport :: Int -> IO ()
    , importKeyUid :: CString
    , importKeyFingerprint :: CString
    }
    deriving (Show)

peekInstallIgnorepkgQuestion
    :: Ptr (AlpmQuestionFor 'AlpmQuestionInstallIgnorepkg)
    -> IO (AlpmQuestionFor 'AlpmQuestionInstallIgnorepkg)
peekInstallIgnorepkgQuestion p = do
    let
        p' :: AlpmQuestionInstallIgnorepkgPtr
        p' = castPtr p
    InstallIgnorepkg
        <$> pure (set__question_install_ignorepkg_t__install p' . fromIntegral)
        <*> get__question_install_ignorepkg_t__pkg p'

peekReplacePkgQuestion
    :: Ptr (AlpmQuestionFor 'AlpmQuestionReplacePkg)
    -> IO (AlpmQuestionFor 'AlpmQuestionReplacePkg)
peekReplacePkgQuestion p = do
    let
        p' :: AlpmQuestionReplacePtr
        p' = castPtr p
    ReplacePkg
        <$> pure (set__question_replace_t__replace p' . fromIntegral)
        <*> get__question_replace_t__oldpkg p'
        <*> get__question_replace_t__newpkg p'
        <*> get__question_replace_t__newdb p'

peekConflictPkgQuestion
    :: Ptr (AlpmQuestionFor 'AlpmQuestionConflictPkg)
    -> IO (AlpmQuestionFor 'AlpmQuestionConflictPkg)
peekConflictPkgQuestion p = do
    let
        p' :: AlpmQuestionConflictPtr
        p' = castPtr p
    ConflictPkg
        <$> pure (set__question_conflict_t__remove p' . fromIntegral)
        <*> get__question_conflict_t__conflict p'

peekCorruptedPkgQuestion
    :: Ptr (AlpmQuestionFor 'AlpmQuestionCorruptedPkg)
    -> IO (AlpmQuestionFor 'AlpmQuestionCorruptedPkg)
peekCorruptedPkgQuestion p = do
    let
        p' :: AlpmQuestionCorruptedPtr
        p' = castPtr p
    CorruptedPkg
        <$> pure (set__question_corrupted_t__remove p' . fromIntegral)
        <*> get__question_corrupted_t__filepath p'
        <*> (toEnum . fromIntegral <$> get__question_corrupted_t__reason p')

peekRemovePkgsQuestion
    :: Ptr (AlpmQuestionFor 'AlpmQuestionRemovePkgs)
    -> IO (AlpmQuestionFor 'AlpmQuestionRemovePkgs)
peekRemovePkgsQuestion p = do
    let
        p' :: AlpmQuestionRemovePkgsPtr
        p' = castPtr p
    RemovePkgs
        <$> pure (set__question_remove_pkgs_t__skip p' . fromIntegral)
        <*> (fromAlpmList =<< get__question_remove_pkgs_t__packages p')

peekSelectProviderQuestion
    :: Ptr (AlpmQuestionFor 'AlpmQuestionSelectProvider)
    -> IO (AlpmQuestionFor 'AlpmQuestionSelectProvider)
peekSelectProviderQuestion p = do
    let
        p' :: AlpmQuestionSelectProviderPtr
        p' = castPtr p
    SelectProvider
        <$> pure (set__question_select_provider_t__use_index p' . fromIntegral)
        <*> get__question_select_provider_t__depend p'
        <*> (fromAlpmList =<< get__question_select_provider_t__providers p')

peekImportKeyQuestion
    :: Ptr (AlpmQuestionFor 'AlpmQuestionImportKey)
    -> IO (AlpmQuestionFor 'AlpmQuestionImportKey)
peekImportKeyQuestion p = do
    let
        p' :: AlpmQuestionImportKeyPtr
        p' = castPtr p
    ImportKey
        <$> pure (set__question_import_key_t__import p' . fromIntegral)
        <*> get__question_import_key_t__uid p'
        <*> get__question_import_key_t__fingerprint p'

--------------------------------------------------------------------------------
-- Packages
--------------------------------------------------------------------------------

deriving instance Eq AlpmPkg

peekAlpmPkg :: AlpmPkgPtr -> IO AlpmPkg
peekAlpmPkg = undefined

-- peekAlpmPkg p =
--    AlpmPkg
--        <$> pkgGetName (AlpmPkgPtr $ castPtr p)
--        <*> ( pkgGetVersion (AlpmPkgPtr $ castPtr p)
--                >>= either throwIO pure . parseVersionFromText . Text.pack
--            )

--------------------------------------------------------------------------------
-- Package versions
--------------------------------------------------------------------------------

instance Eq AlpmVersion where
    x == y = compare x y == EQ

instance Ord AlpmVersion where
    x `compare` y = unsafePerformIO (vercmp' x y)

vercmp :: (MonadIO m) => String -> String -> m Ordering
vercmp xs ys = liftIO $
    withCString xs $ \xs' ->
        withCString ys $ \ys' -> do
            res <- Binding.pkg_vercmp xs' ys'
            pure (res `compare` 0)

vercmp' :: (MonadIO m) => AlpmVersion -> AlpmVersion -> m Ordering
vercmp' x y = vercmp (showAlpmVersion x) (showAlpmVersion y)

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

data AlpmDepend = AlpmDepend
    { alpmDependName :: AlpmPkgName
    , alpmDependConstraint :: AlpmConstraint
    }
    deriving (Eq, Show)

instance Storable AlpmDepend where
    sizeOf _ = sizeof__depend_t
    alignment _ = alignof__depend_t
    peek p =
        AlpmDepend
            <$> (get__depend_t__name p >>= fmap AlpmPkgName . ShortText.unsafePackCString)
            <*> peekConstraint p
    poke p x = do
        n <- ShortText.newCString . unAlpmPkgName . alpmDependName $ x
        set__depend_t__name p n
        pokeConstraint p . alpmDependConstraint $ x

data AlpmConstraint
    = ConstraintAny
    | ConstraintLT AlpmVersion
    | ConstraintLE AlpmVersion
    | ConstraintEQ AlpmVersion
    | ConstraintGE AlpmVersion
    | ConstraintGT AlpmVersion
    deriving (Eq, Show)

constraintToDepmod :: AlpmConstraint -> AlpmDepmod
constraintToDepmod ConstraintAny = AlpmDepModAny
constraintToDepmod ConstraintLT{} = AlpmDepModLt
constraintToDepmod ConstraintLE{} = AlpmDepModLe
constraintToDepmod ConstraintEQ{} = AlpmDepModEq
constraintToDepmod ConstraintGE{} = AlpmDepModGe
constraintToDepmod ConstraintGT{} = AlpmDepModGt

constraintVersion :: AlpmConstraint -> Maybe AlpmVersion
constraintVersion ConstraintAny = Nothing
constraintVersion (ConstraintLT v) = Just v
constraintVersion (ConstraintLE v) = Just v
constraintVersion (ConstraintEQ v) = Just v
constraintVersion (ConstraintGE v) = Just v
constraintVersion (ConstraintGT v) = Just v

peekConstraint :: AlpmDependPtr -> IO AlpmConstraint
peekConstraint p = do
    mversion <-
        get__depend_t__version p
            >>= peekMaybe (either throwIO pure <=< parseVersionFromCString)
    depmod <- toEnum . fromIntegral <$> get__depend_t__mod p
    pure $ case (mversion, depmod) of
        (Nothing, AlpmDepModAny) -> ConstraintAny
        (Just version, AlpmDepModEq) -> ConstraintEQ version
        (Just version, AlpmDepModLe) -> ConstraintLE version
        (Just version, AlpmDepModGe) -> ConstraintGE version
        (Just version, AlpmDepModLt) -> ConstraintLT version
        (Just version, AlpmDepModGt) -> ConstraintGT version
        _ ->
            error
                ("Illegal version/depmod combination: " <> show mversion <> " " <> show depmod)

pokeConstraint :: AlpmDependPtr -> AlpmConstraint -> IO ()
pokeConstraint p x = do
    v <- case constraintVersion x of
        Nothing -> pure nullPtr
        Just v -> newCString (showAlpmVersion v)
    set__depend_t__version p v
    set__depend_t__mod p . fromIntegral . fromEnum . constraintToDepmod $ x

--------------------------------------------------------------------------------
-- Groups
--------------------------------------------------------------------------------

data AlpmGroup = AlpmGroup
    { alpmGroupName :: String
    , alpmGroupPackages :: [AlpmPkgPtr]
    }
    deriving (Show)

peekAlpmGroup :: AlpmGroupPtr -> IO AlpmGroup
peekAlpmGroup p =
    AlpmGroup
        <$> (get__group_t__name p >>= peekCString)
        <*> (get__group_t__packages p >>= fromAlpmList)

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

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

data AlpmConflict = AlpmConflict
    { alpmConflictPkg1 :: AlpmPkg
    , alpmConflictPkg2 :: AlpmPkg
    , alpmConflictReason :: AlpmDepend
    }
    deriving (Eq, Show)

peekAlpmConflict :: AlpmConflictPtr -> IO AlpmConflict
peekAlpmConflict p =
    AlpmConflict
        <$> (get__conflict_t__package1 p >>= peekAlpmPkg)
        <*> (get__conflict_t__package2 p >>= peekAlpmPkg)
        <*> (get__conflict_t__reason p >>= peek)

data AlpmDepmissing = AlpmDepmissing
    { alpmDepmissingTarget :: String
    , alpmDepmissingDepend :: AlpmDepend
    , alpmDepmissingCausingPkg :: Maybe String
    }
    deriving (Eq, Show)

peekAlpmDepmissing :: AlpmDepmissingPtr -> IO AlpmDepmissing
peekAlpmDepmissing p =
    AlpmDepmissing
        <$> (get__depmissing_t__target p >>= peekCString)
        <*> (get__depmissing_t__depend p >>= peek)
        <*> (get__depmissing_t__causingpkg p >>= peekMaybe peekCString)

data AlpmFileconflict = AlpmFileconflict
    { alpmFileconflictTarget :: String
    , alpmFileconflictType :: AlpmFileconflicttype
    , alpmFileconflictFile :: FilePath
    , alpmFileconflictCtarget :: String
    }
    deriving (Eq, Show)

peekAlpmFileconflict :: AlpmFileconflictPtr -> IO AlpmFileconflict
peekAlpmFileconflict p =
    AlpmFileconflict
        <$> (get__fileconflict_t__target p >>= peekCString)
        <*> (get__fileconflict_t__type p <&> toEnum . fromIntegral)
        <*> (get__fileconflict_t__file p >>= peekCString)
        <*> (get__fileconflict_t__ctarget p >>= peekCString)
