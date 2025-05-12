{-# LANGUAGE QuasiQuotes #-}

module RunSpec (spec) where

import Control.Exception (finally)
import Dhall.Core (Chunks (..), Expr (App, TextLit))
import Effectful
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (LogLevel (..), runLog)
import Effectful.Process.Typed (runTypedProcess)
import Effectful.Temporary (runTemporary)
import Log.Data (showLogMessage)
import Log.Logger (mkLogger, shutdownLogger, waitForLogger)
import Path
import Path.IO (
    copyDirRecur,
    doesDirExist,
    ensureDir,
    listDirRecurRel,
    makeAbsolute,
    removeDirRecur,
 )
import Relude hiding (runReader)
import Relude.Extra.Lens (set)
import Test.Hspec

import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified
import Dhall qualified

import Run

default (Text)

spec :: Spec
spec = do
    describe "dhalpm" $ do
        runWith "Empty" [relfile|empty.dhall|]
            $ assertFilesystem
                (
                    [ [reldir|database/|]
                    , [reldir|database/local/|]
                    , [reldir|root/|]
                    ]
                ,
                    [ [relfile|database/local/ALPM_DB_VERSION|]
                    ]
                )

        describe "Install" $ do
            runWith "From syncdb" [relfile|install-from-syncdb.dhall|]
                $ assertFilesystem
                    (
                        [ [reldir|database/|]
                        , [reldir|database/local/|]
                        , [reldir|database/local/test-package-1-1/|]
                        , [reldir|database/sync/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        ]
                    ,
                        [ [relfile|database/local/ALPM_DB_VERSION|]
                        , [relfile|database/local/test-package-1-1/desc|]
                        , [relfile|database/local/test-package-1-1/files|]
                        , [relfile|database/local/test-package-1-1/mtree|]
                        , [relfile|database/sync/testdb.db|]
                        , [relfile|root/file1|]
                        , [relfile|root/file2|]
                        ]
                    )

            runWith "From file" [relfile|install-from-file.dhall|]
                $ assertFilesystem
                    (
                        [ [reldir|database/|]
                        , [reldir|database/local/|]
                        , [reldir|database/local/test-package-1-1/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        ]
                    ,
                        [ [relfile|database/local/ALPM_DB_VERSION|]
                        , [relfile|database/local/test-package-1-1/desc|]
                        , [relfile|database/local/test-package-1-1/files|]
                        , [relfile|database/local/test-package-1-1/mtree|]
                        , [relfile|root/file1|]
                        , [relfile|root/file2|]
                        ]
                    )

            runWith "From build" [relfile|install-from-build.dhall|]
                $ assertFilesystem
                    (
                        [ [reldir|database/|]
                        , [reldir|database/local/|]
                        , [reldir|database/local/test-package-1-1/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        , [reldir|test-package/|]
                        , [reldir|test-package/pkg/|]
                        , [reldir|test-package/pkg/test-package/|]
                        , [reldir|test-package/pkg/test-package/testdir/|]
                        , [reldir|test-package/src/|]
                        ]
                    ,
                        [ [relfile|database/local/ALPM_DB_VERSION|]
                        , [relfile|database/local/test-package-1-1/desc|]
                        , [relfile|database/local/test-package-1-1/files|]
                        , [relfile|database/local/test-package-1-1/mtree|]
                        , [relfile|root/file1|]
                        , [relfile|root/file2|]
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
                $ assertFilesystem
                    (
                        [ [reldir|database/|]
                        , [reldir|database/local/|]
                        , [reldir|database/local/depends-package-1-1/|]
                        , [reldir|database/local/test-package-1-1/|]
                        , [reldir|database/sync/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        ]
                    ,
                        [ [relfile|database/local/ALPM_DB_VERSION|]
                        , [relfile|database/local/depends-package-1-1/desc|]
                        , [relfile|database/local/depends-package-1-1/files|]
                        , [relfile|database/local/depends-package-1-1/mtree|]
                        , [relfile|database/local/test-package-1-1/desc|]
                        , [relfile|database/local/test-package-1-1/files|]
                        , [relfile|database/local/test-package-1-1/mtree|]
                        , [relfile|database/sync/testdb.db|]
                        , [relfile|root/depends-file1|]
                        , [relfile|root/depends-file2|]
                        , [relfile|root/file1|]
                        , [relfile|root/file2|]
                        ]
                    )

            runWith "Providers" [relfile|providers.dhall|]
                $ assertFilesystem
                    (
                        [ [reldir|database/|]
                        , [reldir|database/local/|]
                        , [reldir|database/local/providers-2-1-1/|]
                        , [reldir|database/sync/|]
                        , [reldir|root/|]
                        ]
                    ,
                        [ [relfile|database/local/ALPM_DB_VERSION|]
                        , [relfile|database/local/providers-2-1-1/desc|]
                        , [relfile|database/local/providers-2-1-1/files|]
                        , [relfile|database/local/providers-2-1-1/mtree|]
                        , [relfile|database/sync/testdb.db|]
                        ]
                    )

            runWith'
                "Remove orphaned"
                [relfile|remove-orphaned.dhall|]
                True
                $ assertFilesystem
                    (
                        [ [reldir|database/|]
                        , [reldir|database/local/|]
                        , -- , [reldir|database/local/depends-package-1-1/|]
                          [reldir|database/local/test-package-1-1/|]
                        , [reldir|database/sync/|]
                        , [reldir|root/|]
                        , [reldir|root/testdir/|]
                        ]
                    ,
                        [ [relfile|database/local/ALPM_DB_VERSION|]
                        , -- , [relfile|database/local/depends-package-1-1/desc|]
                          -- , [relfile|database/local/depends-package-1-1/files|]
                          -- , [relfile|database/local/depends-package-1-1/mtree|]
                          [relfile|database/local/test-package-1-1/desc|]
                        , [relfile|database/local/test-package-1-1/files|]
                        , [relfile|database/local/test-package-1-1/mtree|]
                        , [relfile|database/sync/testdb.db|]
                        , -- , [relfile|root/depends-file1|]
                          -- , [relfile|root/depends-file2|]
                          [relfile|root/file1|]
                        , [relfile|root/file2|]
                        ]
                    )

        describe "Real world" $ do
            runWith "Install latest filesystem" [relfile|realworld-simple.dhall|] $ \_ -> do
                return ()

runWith
    :: Text
    -> Path Rel File
    -> (Path Abs Dir -> Expectation)
    -> Spec
runWith name config = runWith' name config False

runWith'
    :: Text
    -> Path Rel File
    -> Bool
    -> (Path Abs Dir -> Expectation)
    -> Spec
runWith' name config setupDirectories action = do
    dataDir <- runIO (makeAbsolute [reldir|test/data|])
    testDir <- runIO $ do
        testDirName <-
            parseRelDir
                . Text.unpack
                . Text.replace " " "_"
                . Text.toLower
                $ name
        makeAbsolute ([reldir|test/.out|] </> testDirName)

    let
        pristineDatabaseDir = dataDir </> [reldir|database|]
        pristineRootDir = dataDir </> [reldir|root|]
        databaseDir = testDir </> [reldir|database|]
        rootDir = testDir </> [reldir|root|]

        setup :: IO ()
        setup = do
            whenM (doesDirExist testDir) (removeDirRecur testDir)
            ensureDir testDir
            if setupDirectories
                then do
                    copyDirRecur pristineDatabaseDir databaseDir
                    copyDirRecur pristineRootDir rootDir
                else do
                    ensureDir databaseDir
                    ensureDir rootDir

    expression <- runIO $ do
        let
            configFile = [reldir|test|] </> config
            rootDirectory = parent configFile
            settings =
                set Dhall.rootDirectory (fromRelDir rootDirectory)
                    . set Dhall.sourceName (fromRelFile configFile)
                    $ Dhall.defaultInputSettings

        text <- Data.Text.IO.readFile (fromRelFile configFile)
        expression <- Dhall.parseWithSettings settings text
        pure
            ( App
                ( App
                    expression
                    (absDirToDhall dataDir)
                )
                (absDirToDhall testDir)
            )

    before_ setup . it (Text.unpack name) $ do
        let
            logFile = testDir </> [relfile|log.txt|]
        withFile (fromAbsFile logFile) WriteMode $ \h -> do
            logger <- mkLogger "file-logger" $ \msg -> do
                Data.Text.IO.hPutStrLn h (showLogMessage Nothing msg)
                hFlush h
            flip finally (waitForLogger logger >> shutdownLogger logger)
                $ runEff
                . runFileSystem
                . runLog "" logger LogTrace
                . runTemporary
                . runTypedProcess
                $ runFromExpression expression
            action testDir
    where
        absDirToDhall :: Path Abs Dir -> Expr s a
        absDirToDhall = TextLit . Chunks [] . Text.pack . fromAbsDir

assertFilesystem
    :: ([Path Rel Dir], [Path Rel File])
    -> Path Abs Dir
    -> Expectation
assertFilesystem (expectedDirectories, expectedFiles) baseDir = do
    (actualDirectories, actualFiles) <- listDirRecurRel baseDir
    let
        actual = (List.sort actualDirectories, List.sort actualFiles)
        expected =
            ( List.sort expectedDirectories
            , List.sort ([relfile|log.txt|] : expectedFiles)
            )
    actual `shouldBe` expected
