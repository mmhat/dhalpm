{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Archlinux.Alpm.Binding
    ( -- * Handle
      AlpmHandlePtr (..)
    , AlpmSiglevel(..)
    , initialize
    , release
    , sync_sysupgrade
    , option_set_eventcb
    , option_set_questioncb
    , get_localdb
    , get_syncdbs
    , register_syncdb
    , db_update
    , find_dbs_satisfier
    , AlpmCbEventFunPtr
    , AlpmCbQuestionFunPtr

      -- ** Events
    , AlpmEventFor
    , AlpmEventPtr
    , AlpmEventDatabaseMissingPtr
    , AlpmEventHookPtr
    , AlpmEventHookRunPtr
    , AlpmEventOptdepRemovalPtr
    , AlpmEventPackageOperationPtr
    , AlpmEventPacnewCreatedPtr
    , AlpmEventPacsaveCreatedPtr
    , AlpmEventPkgdownloadPtr
    , AlpmEventPkgRetrievePtr
    , AlpmEventScriptletInfoPtr
    , AlpmEventType(..)
    , AlpmHookWhen(..)
    , AlpmPackageOperation(..)
    , get__event_t__type
    , get__event_database_missing_t__dbname
    , get__event_hook_t__when
    , get__event_hook_run_t__position
    , get__event_hook_run_t__total
    , get__event_hook_run_t__name
    , get__event_hook_run_t__desc
    , get__event_optdep_removal_t__pkg
    , get__event_optdep_removal_t__optdep
    , get__event_package_operation_t__operation

      -- ** Questions
    , AlpmQuestionFor
    , AlpmQuestionPtr
    , AlpmQuestionSelectProviderPtr
    , AlpmQuestionType(..)
    , get__question_t__type
    , get__question_select_provider_t__depend
    , get__question_select_provider_t__providers
    , set__question_select_provider_t__use_index

      -- * Databases
    , AlpmDbPtr
    , AlpmDbUsage(..)
    , db_get_group
    , db_get_servers
    , db_add_server
    , db_remove_server
    , db_set_servers
    , db_set_usage
    , db_get_name
    , db_get_pkg
    , db_get_pkgcache
    , db_get_groupcache
    , db_search

      -- * Package groups
    , AlpmGroupPtr
    , get__group_t__name
    , get__group_t__packages

      -- * Packages
    , AlpmPkgPtr
    , AlpmPkgfrom(..)
    , AlpmPkgreason(..)
    , AlpmBackupPtr
    , AlpmFilelistPtr
    , AlpmFilePtr
    , pkg_load
    , pkg_get_name
    , pkg_get_db
    , pkg_get_filename
    , pkg_get_version
    , pkg_get_origin
    , pkg_get_provides
    , pkg_get_depends
    , pkg_get_reason
    , pkg_set_reason
    , pkg_free

      -- * Dependencies
    , AlpmDependPtr
    , AlpmDepmod(..)
    , dep_from_string
    , dep_compute_string
    , dep_free
    , sizeof__depend_t
    , alignof__depend_t
    , get__depend_t__name
    , set__depend_t__name
    , get__depend_t__mod
    , set__depend_t__mod
    , get__depend_t__version
    , set__depend_t__version

      -- * Transactions
    , AlpmTransflag(..)
    , trans_init
    , add_pkg
    , remove_pkg
    , trans_prepare
    , trans_get_add
    , trans_get_remove
    , trans_commit
    , trans_interrupt
    , trans_release

      -- ** Conflicts
    , AlpmConflictPtr
    , conflict_free
    , get__conflict_t__package1
    , get__conflict_t__package2
    , get__conflict_t__reason

      -- ** Missing dependencies
    , AlpmDepmissingPtr
    , depmissing_free
    , get__depmissing_t__target
    , get__depmissing_t__depend
    , get__depmissing_t__causingpkg

      -- ** File conflicts
    , AlpmFileconflictPtr
    , AlpmFileconflicttype(..)
    , fileconflict_free
    , get__fileconflict_t__target
    , get__fileconflict_t__type
    , get__fileconflict_t__file
    , get__fileconflict_t__ctarget

      -- * Errors and utilities
    , pkg_vercmp

      -- ** Errors
    , AlpmErrNo(..)
    , AlpmErrNoPtr
    , errno
    , strerror

      -- ** Lists
    , AlpmListPtr
    , list_add
    , list_free
    , get__list_t__data
    , get__list_t__next
    )where

import Prelude

import GHC.Generics (Generic)
import Foreign
import Foreign.C
import Data.Kind (Type)

import Archlinux.Alpm.Package.Types (AlpmPkg)
import {-# SOURCE #-} Archlinux.Alpm.Types
    ( AlpmConflict
    , AlpmDepend
    , AlpmDepmissing
    , AlpmEvent
    , AlpmFileconflict
    , AlpmGroup
    , AlpmQuestion
    )

#include <alpm.h>

{#context prefix = "alpm" add prefix = "Alpm" #}

{#enum alpm_db_usage_t          as AlpmDbUsage          {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_depmod_t            as AlpmDepmod           {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_errno_t             as AlpmErrNo            {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_event_type_t        as AlpmEventType        {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_hook_when_t         as AlpmHookWhen         {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_fileconflicttype_t  as AlpmFileconflicttype {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_package_operation_t as AlpmPackageOperation {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_pkgfrom_t           as AlpmPkgfrom          {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_pkgreason_t         as AlpmPkgreason        {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_question_type_t     as AlpmQuestionType     {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_siglevel_t          as AlpmSiglevel         {underscoreToCase} deriving (Eq, Generic, Show) #}
{#enum alpm_transflag_t         as AlpmTransflag        {underscoreToCase} deriving (Eq, Generic, Show) #}

{#pointer *alpm_backup_t                   as AlpmBackupPtr                                     #}
{#pointer  alpm_cb_event                   as AlpmCbEventFunPtr                                 #}
{#pointer  alpm_cb_question                as AlpmCbQuestionFunPtr                              #}
{#pointer *alpm_conflict_t                 as AlpmConflictPtr               -> AlpmConflict     #}
{#pointer *alpm_db_t                       as AlpmDbPtr                                         #}
{#pointer *alpm_depend_t                   as AlpmDependPtr                 -> AlpmDepend       #}
{#pointer *alpm_depmissing_t               as AlpmDepmissingPtr             -> AlpmDepmissing   #}
{#pointer *alpm_errno_t                    as AlpmErrNoPtr                                      #}
{#pointer *alpm_event_t                    as AlpmEventPtr                  -> AlpmEvent        #}
{#pointer *alpm_event_package_operation_t  as AlpmEventPackageOperationPtr                      #}
{#pointer *alpm_event_optdep_removal_t     as AlpmEventOptdepRemovalPtr                         #}
{#pointer *alpm_event_scriptlet_info_t     as AlpmEventScriptletInfoPtr                         #}
{#pointer *alpm_event_database_missing_t   as AlpmEventDatabaseMissingPtr                       #}
{#pointer *alpm_event_pkgdownload_t        as AlpmEventPkgdownloadPtr                           #}
{#pointer *alpm_event_pacnew_created_t     as AlpmEventPacnewCreatedPtr                         #}
{#pointer *alpm_event_pacsave_created_t    as AlpmEventPacsaveCreatedPtr                        #}
{#pointer *alpm_event_hook_t               as AlpmEventHookPtr                                  #}
{#pointer *alpm_event_hook_run_t           as AlpmEventHookRunPtr                               #}
{#pointer *alpm_event_pkg_retrieve_t       as AlpmEventPkgRetrievePtr                           #}
{#pointer *alpm_file_t                     as AlpmFilePtr                                       #}
{#pointer *alpm_fileconflict_t             as AlpmFileconflictPtr           -> AlpmFileconflict #}
{#pointer *alpm_filelist_t                 as AlpmFilelistPtr                                   #}
{#pointer *alpm_group_t                    as AlpmGroupPtr                  -> AlpmGroup        #}
{#pointer *alpm_handle_t                   as AlpmHandlePtr                 newtype             #}
{#pointer *alpm_list_t                     as AlpmListPtr                                       #}
{#pointer *alpm_pkg_t                      as AlpmPkgPtr                    -> AlpmPkg          #}
{#pointer *alpm_question_t                 as AlpmQuestionPtr               -> AlpmQuestion     #}
{#pointer *alpm_question_select_provider_t as AlpmQuestionSelectProviderPtr                     #}

--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

initialize :: CString -> CString -> AlpmErrNoPtr -> IO AlpmHandlePtr
initialize = {#call alpm_initialize #}

release :: AlpmHandlePtr -> IO CInt
release = {#call alpm_release #}

sync_sysupgrade :: AlpmHandlePtr -> CInt -> IO CInt
sync_sysupgrade = {#call alpm_sync_sysupgrade #}

option_set_eventcb :: AlpmHandlePtr -> AlpmCbEventFunPtr -> Ptr () -> IO CInt
option_set_eventcb = {#call alpm_option_set_eventcb #}

option_set_questioncb :: AlpmHandlePtr -> AlpmCbQuestionFunPtr -> Ptr () -> IO CInt
option_set_questioncb = {#call alpm_option_set_questioncb #}

get_localdb :: AlpmHandlePtr -> IO AlpmDbPtr
get_localdb = {#call alpm_get_localdb #}

get_syncdbs :: AlpmHandlePtr -> IO AlpmListPtr
get_syncdbs = {#call alpm_get_syncdbs #}

register_syncdb :: AlpmHandlePtr -> CString -> CInt -> IO AlpmDbPtr
register_syncdb = {#call alpm_register_syncdb #}

db_update :: AlpmHandlePtr -> AlpmListPtr -> CInt -> IO CInt
db_update = {#call alpm_db_update #}

find_dbs_satisfier :: AlpmHandlePtr -> AlpmListPtr -> CString -> IO AlpmPkgPtr
find_dbs_satisfier = {#call alpm_find_dbs_satisfier #}

----------------------------------------
-- alpm_event_t and friends
----------------------------------------

data family AlpmEventFor (a :: AlpmEventType) :: Type

get__event_t__type :: AlpmEventPtr -> IO CInt
get__event_t__type = {#get alpm_event_t->type #}

get__event_database_missing_t__dbname :: AlpmEventDatabaseMissingPtr -> IO CString
get__event_database_missing_t__dbname = {#get alpm_event_database_missing_t->dbname #}

get__event_hook_t__when :: AlpmEventHookPtr -> IO CInt
get__event_hook_t__when = {#get alpm_event_hook_t->when #}

get__event_hook_run_t__position :: AlpmEventHookRunPtr -> IO CULong
get__event_hook_run_t__position = {#get alpm_event_hook_run_t->position #}

get__event_hook_run_t__total :: AlpmEventHookRunPtr -> IO CULong
get__event_hook_run_t__total = {#get alpm_event_hook_run_t->total #}

get__event_hook_run_t__name :: AlpmEventHookRunPtr -> IO CString
get__event_hook_run_t__name = {#get alpm_event_hook_run_t->name #}

get__event_hook_run_t__desc :: AlpmEventHookRunPtr -> IO CString
get__event_hook_run_t__desc = {#get alpm_event_hook_run_t->desc #}

get__event_optdep_removal_t__pkg :: AlpmEventOptdepRemovalPtr -> IO AlpmPkgPtr
get__event_optdep_removal_t__pkg = {#get alpm_event_optdep_removal_t->pkg #}

get__event_optdep_removal_t__optdep :: AlpmEventOptdepRemovalPtr -> IO AlpmDependPtr
get__event_optdep_removal_t__optdep = {#get alpm_event_optdep_removal_t->optdep #}

get__event_package_operation_t__operation :: AlpmEventPackageOperationPtr -> IO CInt
get__event_package_operation_t__operation = {#get alpm_event_package_operation_t->operation #}

----------------------------------------
-- alpm_question_t and friends
----------------------------------------

data family AlpmQuestionFor (a :: AlpmQuestionType) :: Type

get__question_t__type :: AlpmQuestionPtr -> IO CInt
get__question_t__type = {#get alpm_question_t->type #}

get__question_select_provider_t__depend :: AlpmQuestionSelectProviderPtr -> IO AlpmDependPtr
get__question_select_provider_t__depend = {#get alpm_question_select_provider_t->depend #}

get__question_select_provider_t__providers :: AlpmQuestionSelectProviderPtr -> IO AlpmListPtr
get__question_select_provider_t__providers = {#get alpm_question_select_provider_t->providers #}

set__question_select_provider_t__use_index :: AlpmQuestionSelectProviderPtr -> CInt -> IO ()
set__question_select_provider_t__use_index = {#set alpm_question_select_provider_t->use_index #}

--------------------------------------------------------------------------------
-- Databases
--------------------------------------------------------------------------------

db_get_group :: AlpmDbPtr -> CString -> IO AlpmGroupPtr
db_get_group = {#call alpm_db_get_group #}

db_get_servers :: AlpmDbPtr -> IO AlpmListPtr
db_get_servers = {#call alpm_db_get_servers #}

db_add_server :: AlpmDbPtr -> CString -> IO CInt
db_add_server = {#call alpm_db_add_server #}

db_remove_server :: AlpmDbPtr -> CString -> IO CInt
db_remove_server = {#call alpm_db_remove_server #}

db_set_servers :: AlpmDbPtr -> AlpmListPtr -> IO CInt
db_set_servers = {#call alpm_db_set_servers #}

db_set_usage :: AlpmDbPtr -> CInt -> IO CInt
db_set_usage = {#call alpm_db_set_usage #}

db_get_name :: AlpmDbPtr -> IO CString
db_get_name = {#call alpm_db_get_name #}

db_get_pkg :: AlpmDbPtr -> CString -> IO AlpmPkgPtr
db_get_pkg = {#call alpm_db_get_pkg #}

db_get_pkgcache :: AlpmDbPtr -> IO AlpmListPtr
db_get_pkgcache = {#call alpm_db_get_pkgcache #}

db_get_groupcache :: AlpmDbPtr -> IO AlpmListPtr
db_get_groupcache = {#call alpm_db_get_groupcache #}

db_search :: AlpmDbPtr -> AlpmListPtr -> Ptr AlpmListPtr -> IO CInt
db_search = {#call alpm_db_search #}

--------------------------------------------------------------------------------
-- Package groups
--------------------------------------------------------------------------------

----------------------------------------
-- alpm_group_t
----------------------------------------

get__group_t__name :: AlpmGroupPtr -> IO CString
get__group_t__name = {#get alpm_group_t->name #}

get__group_t__packages :: AlpmGroupPtr -> IO AlpmListPtr
get__group_t__packages = {#get alpm_group_t->packages #}

--------------------------------------------------------------------------------
-- Packages
--------------------------------------------------------------------------------

pkg_load :: AlpmHandlePtr -> CString -> CInt -> CInt -> Ptr AlpmPkgPtr -> IO CInt
pkg_load = {#call alpm_pkg_load #}

pkg_get_name :: AlpmPkgPtr -> IO CString
pkg_get_name = {#call alpm_pkg_get_name #}

pkg_get_db :: AlpmPkgPtr -> IO AlpmDbPtr
pkg_get_db = {#call alpm_pkg_get_db #}

pkg_get_filename :: AlpmPkgPtr -> IO CString
pkg_get_filename = {#call alpm_pkg_get_filename #}

pkg_get_version :: AlpmPkgPtr -> IO CString
pkg_get_version = {#call alpm_pkg_get_version #}

pkg_get_origin :: AlpmPkgPtr -> IO CInt
pkg_get_origin = {#call alpm_pkg_get_origin #}

pkg_get_provides :: AlpmPkgPtr -> IO AlpmListPtr
pkg_get_provides = {#call alpm_pkg_get_provides #}

pkg_get_depends :: AlpmPkgPtr -> IO AlpmListPtr
pkg_get_depends = {#call alpm_pkg_get_depends #}

pkg_get_reason :: AlpmPkgPtr -> IO CInt
pkg_get_reason = {#call alpm_pkg_get_reason #}

pkg_set_reason :: AlpmPkgPtr -> CInt -> IO CInt
pkg_set_reason = {#call alpm_pkg_set_reason #}

pkg_free :: AlpmPkgPtr -> IO CInt
pkg_free = {#call alpm_pkg_free #}

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

dep_from_string :: CString -> IO AlpmDependPtr
dep_from_string = {#call alpm_dep_from_string #}

dep_compute_string :: AlpmDependPtr -> IO CString
dep_compute_string = {#call alpm_dep_compute_string #}

dep_free :: AlpmDependPtr -> IO ()
dep_free = {#call alpm_dep_free #}

----------------------------------------
-- alpm_depend_t
----------------------------------------

sizeof__depend_t :: Int
sizeof__depend_t = {#sizeof alpm_depend_t #}

alignof__depend_t :: Int
alignof__depend_t = {#alignof alpm_depend_t #}

get__depend_t__name :: AlpmDependPtr -> IO CString
get__depend_t__name = {#get alpm_depend_t->name #}

set__depend_t__name :: AlpmDependPtr -> CString -> IO ()
set__depend_t__name = {#set alpm_depend_t->name #}

get__depend_t__mod :: AlpmDependPtr -> IO CInt
get__depend_t__mod = {#get alpm_depend_t->mod #}

set__depend_t__mod :: AlpmDependPtr -> CInt -> IO ()
set__depend_t__mod = {#set alpm_depend_t->mod #}

get__depend_t__version :: AlpmDependPtr -> IO CString
get__depend_t__version = {#get alpm_depend_t->version #}

set__depend_t__version :: AlpmDependPtr -> CString -> IO ()
set__depend_t__version = {#set alpm_depend_t->version #}

--------------------------------------------------------------------------------
-- Transactions
--------------------------------------------------------------------------------

trans_init :: AlpmHandlePtr -> CInt -> IO CInt
trans_init = {#call alpm_trans_init #}

add_pkg :: AlpmHandlePtr -> AlpmPkgPtr -> IO CInt
add_pkg = {#call alpm_add_pkg #}

remove_pkg :: AlpmHandlePtr -> AlpmPkgPtr -> IO CInt
remove_pkg = {#call alpm_remove_pkg #}

trans_prepare :: AlpmHandlePtr -> Ptr AlpmListPtr -> IO CInt
trans_prepare = {#call alpm_trans_prepare #}

trans_get_add :: AlpmHandlePtr -> IO AlpmListPtr
trans_get_add = {#call alpm_trans_get_add #}

trans_get_remove :: AlpmHandlePtr -> IO AlpmListPtr
trans_get_remove = {#call alpm_trans_get_remove #}

trans_commit :: AlpmHandlePtr -> Ptr AlpmListPtr -> IO CInt
trans_commit = {#call alpm_trans_commit #}

trans_interrupt :: AlpmHandlePtr -> IO CInt
trans_interrupt = {#call alpm_trans_interrupt #}

trans_release :: AlpmHandlePtr -> IO CInt
trans_release = {#call alpm_trans_release #}

conflict_free :: AlpmConflictPtr -> IO ()
conflict_free = {#call alpm_conflict_free #}

depmissing_free :: AlpmDepmissingPtr -> IO ()
depmissing_free = {#call alpm_depmissing_free #}

fileconflict_free :: AlpmFileconflictPtr -> IO ()
fileconflict_free = {#call alpm_fileconflict_free #}

----------------------------------------
-- alpm_conflict_t
----------------------------------------

get__conflict_t__package1 :: AlpmConflictPtr -> IO AlpmPkgPtr
get__conflict_t__package1 = {#get alpm_conflict_t->package1 #}

get__conflict_t__package2 :: AlpmConflictPtr -> IO AlpmPkgPtr
get__conflict_t__package2 = {#get alpm_conflict_t->package2 #}

get__conflict_t__reason :: AlpmConflictPtr -> IO AlpmDependPtr
get__conflict_t__reason = {#get alpm_conflict_t->reason #}

----------------------------------------
-- alpm_depmissing_t
----------------------------------------

get__depmissing_t__target :: AlpmDepmissingPtr -> IO CString
get__depmissing_t__target = {#get alpm_depmissing_t->target #}

get__depmissing_t__depend :: AlpmDepmissingPtr -> IO AlpmDependPtr
get__depmissing_t__depend = {#get alpm_depmissing_t->depend #}

get__depmissing_t__causingpkg :: AlpmDepmissingPtr -> IO CString
get__depmissing_t__causingpkg = {#get alpm_depmissing_t->causingpkg #}

----------------------------------------
-- alpm_fileconflict_t
----------------------------------------

get__fileconflict_t__target :: AlpmFileconflictPtr -> IO CString
get__fileconflict_t__target = {#get alpm_fileconflict_t->target #}

get__fileconflict_t__type :: AlpmFileconflictPtr -> IO CInt
get__fileconflict_t__type = {#get alpm_fileconflict_t->type #}

get__fileconflict_t__file :: AlpmFileconflictPtr -> IO CString
get__fileconflict_t__file = {#get alpm_fileconflict_t->file #}

get__fileconflict_t__ctarget :: AlpmFileconflictPtr -> IO CString
get__fileconflict_t__ctarget = {#get alpm_fileconflict_t->ctarget #}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

pkg_vercmp :: CString -> CString -> IO CInt
pkg_vercmp = {#call alpm_pkg_vercmp #}

errno :: AlpmHandlePtr -> IO CInt
errno = {#call alpm_errno #}

strerror :: CInt -> IO CString
strerror = {#call alpm_strerror #}

list_add :: AlpmListPtr -> Ptr () -> IO AlpmListPtr
list_add = {#call alpm_list_add #}

list_free :: AlpmListPtr -> IO ()
list_free = {#call alpm_list_free #}

----------------------------------------
-- alpm_list_t
----------------------------------------

get__list_t__data :: AlpmListPtr -> IO (Ptr ())
get__list_t__data = {#get alpm_list_t->data #}

get__list_t__next :: AlpmListPtr -> IO AlpmListPtr
get__list_t__next = {#get alpm_list_t->next #}
