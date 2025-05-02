{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Import (
    module Export,
    doesFileExist,
    ensureDir,
    resolveDir',
    resolveFile',
    withSystemTempFile,
) where

import Effectful as Export
import Effectful.Dispatch.Static (unsafeEff_, unsafeSeqUnliftIO)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem as Export (FileSystem)
import Path as Export
import Relude as Export hiding (
    Any,
    Reader,
    asks,
 )

import Path.IO qualified

import Types as Export

--------------------------------------------------------------------------------
-- Effectful versions of Path.IO functions
--------------------------------------------------------------------------------

doesFileExist :: (FileSystem :> es) => Path b File -> Eff es Bool
doesFileExist = unsafeEff_ . Path.IO.doesFileExist

ensureDir :: (FileSystem :> es) => Path b Dir -> Eff es ()
ensureDir = unsafeEff_ . Path.IO.ensureDir

resolveDir' :: (FileSystem :> es) => FilePath -> Eff es (Path Abs Dir)
resolveDir' = unsafeEff_ . Path.IO.resolveDir'

resolveFile' :: (FileSystem :> es) => FilePath -> Eff es (Path Abs File)
resolveFile' = unsafeEff_ . Path.IO.resolveFile'

withSystemTempFile
    :: (FileSystem :> es)
    => String
    -> (Path Abs File -> Handle -> Eff es a)
    -> Eff es a
withSystemTempFile xs f =
    unsafeSeqUnliftIO $ \runInIO ->
        Path.IO.withSystemTempFile xs (\fp h -> runInIO (f fp h))
