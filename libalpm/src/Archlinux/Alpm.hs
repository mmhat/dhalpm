{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Archlinux.Alpm
    ( Constraint(..)
    , Depend(..)
    , DependPtr
    , Depmissing(..)
    , Depmod(..)
    , Group(..)
    , Error(..)
    , UnknownError(..)
    , DbError(..)
    , PkgError(..)
    , TransactionError(..)
    , ErrNo(..)
    , Event(..)
    , EventType(..)
    , HandlePtr
    , DbPtr
    , Pkg(..)
    , Pkgfrom(..)
    , PkgPtr
    , Pkgreason(..)
    , Question(..)
    , Siglevel(..)
    , Transflag(..)
    , UpdateResult(..)
    --
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

      -- * Names
    , PackageName(..)
    , emptyPackageName
    , parsePackageName
    , packageNameParser
    , PackageNameParseException(..)

      -- * Version
    , Version(..)
    , VersionString(..)
    , parseVersion
    , versionParser
    , showVersion
    , VersionParseException(..)

      -- * Low-level utils
    -- , fromAlpmList
    -- , toAlpmList
    -- , withAlpmList

      -- * Exposed for testing only
    , dbGetName
    , withCStrings
    ) where

import Archlinux.Alpm.Binding
import Archlinux.Alpm.Names
