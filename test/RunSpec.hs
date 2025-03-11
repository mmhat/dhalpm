{-# LANGUAGE QuasiQuotes #-}

module RunSpec (spec) where

import Effectful
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (LogLevel (..), runLog)
import Effectful.Process.Typed (runTypedProcess)
import Effectful.Reader.Static (runReader)
import Log.Backend.StandardOutput (withStdOutLogger)
import Path
import Path.IO
import Relude hiding (runReader)
import System.FilePath (dropExtensions)
import Test.Hspec

import Data.List qualified as List

import Run
import Types

default (Text)

spec :: Spec
spec = do
    describe "dhalpm" $ do
        runWith "Empty" [relfile|empty.dhall|]
            $ withFs
                (
                    [ [reldir|db/|]
                    , [reldir|db/local/|]
                    , [reldir|root/|]
                    ]
                ,
                    [ [relfile|db/local/ALPM_DB_VERSION|]
                    ]
                )

        describe "Install" $ do
            runWith "From syncdb" [relfile|install-from-syncdb.dhall|]
                $ withFs
                    (
                        [ [reldir|db/|]
                        , [reldir|db/local/|]
                        , [reldir|db/local/test-package-1-1/|]
                        , [reldir|db/sync/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        ]
                    ,
                        [ [relfile|db/local/ALPM_DB_VERSION|]
                        , [relfile|db/local/test-package-1-1/desc|]
                        , [relfile|db/local/test-package-1-1/files|]
                        , [relfile|db/local/test-package-1-1/mtree|]
                        , [relfile|db/sync/testdb.db|]
                        , [relfile|root/file1|]
                        , [relfile|root/file2|]
                        ]
                    )

            runWith "From file" [relfile|install-from-file.dhall|]
                $ withFs
                    (
                        [ [reldir|db/|]
                        , [reldir|db/local/|]
                        , [reldir|db/local/test-package-1-1/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        ]
                    ,
                        [ [relfile|db/local/ALPM_DB_VERSION|]
                        , [relfile|db/local/test-package-1-1/desc|]
                        , [relfile|db/local/test-package-1-1/files|]
                        , [relfile|db/local/test-package-1-1/mtree|]
                        , [relfile|root/file1|]
                        , [relfile|root/file2|]
                        ]
                    )

            runWith "From build" [relfile|install-from-build.dhall|]
                $ withFs
                    (
                        [ [reldir|db/|]
                        , [reldir|db/local/|]
                        , [reldir|db/local/test-package-1-1/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        , [reldir|test-package/|]
                        , [reldir|test-package/pkg/|]
                        , [reldir|test-package/pkg/test-package/|]
                        , [reldir|test-package/pkg/test-package/testdir/|]
                        , [reldir|test-package/src/|]
                        ]
                    ,
                        [ [relfile|db/local/ALPM_DB_VERSION|]
                        , [relfile|db/local/test-package-1-1/desc|]
                        , [relfile|db/local/test-package-1-1/files|]
                        , [relfile|db/local/test-package-1-1/mtree|]
                        , [relfile|root/file1|]
                        , [relfile|root/file2|]
                        , [relfile|test-package/.SRCINFO|]
                        , [relfile|test-package/PKGBUILD|]
                        , [relfile|test-package/pkg/test-package/.BUILDINFO|]
                        , [relfile|test-package/pkg/test-package/.MTREE|]
                        , [relfile|test-package/pkg/test-package/.PKGINFO|]
                        , [relfile|test-package/pkg/test-package/file1|]
                        , [relfile|test-package/pkg/test-package/file2|]
                        , [relfile|test-package/test-package-1-1-any.pkg.tar.zst|]
                        ]
                    )

            runWith "With dependency" [relfile|install-with-dependency.dhall|]
                $ withFs
                    (
                        [ [reldir|db/|]
                        , [reldir|db/local/|]
                        , [reldir|db/local/depends-package-1-1/|]
                        , [reldir|db/local/test-package-1-1/|]
                        , [reldir|db/sync/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        ]
                    ,
                        [ [relfile|db/local/ALPM_DB_VERSION|]
                        , [relfile|db/local/depends-package-1-1/desc|]
                        , [relfile|db/local/depends-package-1-1/files|]
                        , [relfile|db/local/depends-package-1-1/mtree|]
                        , [relfile|db/local/test-package-1-1/desc|]
                        , [relfile|db/local/test-package-1-1/files|]
                        , [relfile|db/local/test-package-1-1/mtree|]
                        , [relfile|db/sync/testdb.db|]
                        , [relfile|root/depends-file1|]
                        , [relfile|root/depends-file2|]
                        , [relfile|root/file1|]
                        , [relfile|root/file2|]
                        ]
                    )

            runWith "Providers" [relfile|providers.dhall|]
                $ withFs
                    (
                        [ [reldir|db/|]
                        , [reldir|db/local/|]
                        , [reldir|db/local/providers-2-1-1/|]
                        , [reldir|db/sync/|]
                        , [reldir|root/|]
                        ]
                    ,
                        [ [relfile|db/local/ALPM_DB_VERSION|]
                        , [relfile|db/local/providers-2-1-1/desc|]
                        , [relfile|db/local/providers-2-1-1/files|]
                        , [relfile|db/local/providers-2-1-1/mtree|]
                        , [relfile|db/sync/testdb.db|]
                        ]
                    )

            runWith'
                "Remove orphaned"
                [relfile|remove-orphaned.dhall|]
                (copyDatabase [reldir|localdb|])
                $ withFs
                    (
                        [ [reldir|db/|]
                        , [reldir|db/local/|]
                        , -- , [reldir|db/local/depends-package-1-1/|]
                          [reldir|db/local/test-package-1-1/|]
                        , [reldir|db/sync/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        ]
                    ,
                        [ [relfile|db/local/ALPM_DB_VERSION|]
                        , -- , [relfile|db/local/depends-package-1-1/desc|]
                          -- , [relfile|db/local/depends-package-1-1/files|]
                          -- , [relfile|db/local/depends-package-1-1/mtree|]
                          [relfile|db/local/test-package-1-1/desc|]
                        , [relfile|db/local/test-package-1-1/files|]
                        , [relfile|db/local/test-package-1-1/mtree|]
                        , [relfile|db/sync/testdb.db|]
                        , -- , [relfile|root/depends-file1|]
                          -- , [relfile|root/depends-file2|]
                          [relfile|root/file1|]
                        , [relfile|root/file2|]
                        ]
                    )

        describe "Real world" $ do
            runWith "Install latest filesystem" [relfile|realworld-simple.dhall|] $ \_ -> do
                return ()

runWith :: String -> Path Rel File -> (Path Rel Dir -> Expectation) -> Spec
runWith n config = runWith' n config (const $ return ())

runWith'
    :: String
    -> Path Rel File
    -> (Path Rel Dir -> IO ())
    -> (Path Rel Dir -> Expectation)
    -> Spec
runWith' n config customSetup k = before_ setup $ it n $ do
    let
        options =
            Options
                { optionsConfig = Just $ fromRelFile $ [reldir|test|] </> config
                , optionsLogLevel = LogTrace
                , optionsVerbose = True
                }
    withStdOutLogger $ \logger ->
        runEff
            . runReader options
            . runFileSystem
            . runLog "" logger (optionsLogLevel options)
            . runTypedProcess
            $ run
    dir <- parseRelDir $ dropExtensions $ fromRelFile config
    k $ [reldir|test/.out|] </> dir
    where
        setup = do
            dir <- parseRelDir $ dropExtensions $ fromRelFile config
            let
                dir' = [reldir|test/.out|] </> dir
            whenM (doesDirExist dir')
                $ removeDirRecur dir'
            customSetup dir'
            ensureDir $ dir' </> [reldir|root|]

withFs :: ([Path Rel Dir], [Path Rel File]) -> Path b Dir -> Expectation
withFs ref dir = do
    res <- listDirRecurRel dir
    bimap List.sort List.sort res `shouldBe` ref

copyDatabase :: Path Rel Dir -> Path Rel Dir -> IO ()
copyDatabase src = copyDirRecur ([reldir|test/databases|] </> src)
