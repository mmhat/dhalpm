module Import (
    module Export,
    withSystemTempFile,
) where

import Data.Kind as Export (Type)
import Path as Export
import Path.IO as Export hiding (withSystemTempFile)
import RIO as Export hiding (withSystemTempFile, withTempFile)

import Path.IO qualified

import Types as Export
import Types.Dhall as Export

withSystemTempFile
    :: (MonadUnliftIO m) => String -> (Path Abs File -> Handle -> m a) -> m a
withSystemTempFile xs f = do
    u <- askUnliftIO
    liftIO $ Path.IO.withSystemTempFile xs (\fp h -> unliftIO u $ f fp h)
