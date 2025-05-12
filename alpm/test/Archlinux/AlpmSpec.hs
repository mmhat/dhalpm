{-# LANGUAGE QuasiQuotes #-}

module Archlinux.AlpmSpec where

import Foreign hiding (void)
import Path
import Path.IO
import Relude
import Test.Hspec
import Test.QuickCheck

import Data.Text qualified as Text

import Archlinux.Alpm

import Archlinux.Alpm.Package.Types qualified as Package

spec :: Spec
spec = do
    -- TODO: Parsing package names
    -- TODO: Parsing package versions

    describe "alpm_list_t" $ do
        it "fromAlpmList . toAlpmList" . property $ do
            \lst -> withCStrings lst $ \lst' -> do
                res <- withAlpmList lst' fromAlpmList
                res `shouldBe` lst'

    describe "Alpm Dependencies" $ do
        it "peek . poke" $ do
            let
                ref =
                    AlpmDepend
                        { alpmDependName = [Package.name|pkg|]
                        , alpmDependConstraint = ConstraintAny
                        }
            res <- alloca $ \p -> do
                poke p ref
                peek p
            res `shouldBe` ref

        it "depComputeString . depFromString" $ do
            let
                ref = "pkg"
            p <- depFromString ref
            res <- depComputeString p
            depFree p
            res `shouldBe` ref

        it "Compute depstring from AlpmDepend" $ do
            let
                x =
                    AlpmDepend
                        { alpmDependName = [Package.name|pkg|]
                        , alpmDependConstraint = ConstraintAny
                        }
                ref = "pkg"
            res <- alloca $ \p -> do
                poke p x
                depComputeString p
            res `shouldBe` ref

    describe "Alpm databases" $ do
        testAlpm "Local db name" False $ \_getPackageFile _getSyncdbUrl h -> do
            let
                ref = "local"
            db <- getLocaldb h
            dbGetName db `shouldReturn` ref

        testAlpm "Sync db name" False $ \_getPackageFile _getSyncdbUrl h -> do
            let
                ref = "testdb"
            db <- registerSyncdb h "testdb" []
            dbGetName db `shouldReturn` ref

        testAlpm "Double register sync db" False $ \_getPackageFile _getSyncdbUrl h -> do
            let
                ref = ["testdb"]
            void $ registerSyncdb h "testdb" []
            void $ registerSyncdb h "testdb" []
            (getSyncdbs h >>= mapM dbGetName) `shouldReturn` ref

        testAlpm "Register sync db ordering" False $ \_getPackageFile _getSyncdbUrl h -> do
            let
                ref = ["testdb", "testdb2", "testdb3"]
            void $ registerSyncdb h "testdb" []
            void $ registerSyncdb h "testdb2" []
            void $ registerSyncdb h "testdb" []
            void $ registerSyncdb h "testdb3" []
            (getSyncdbs h >>= mapM dbGetName) `shouldReturn` ref

        testAlpm "Update sync db" False $ \_getPackageFile getSyncdbUrl h -> do
            let
                ref =
                    ( []
                    ,
                        [ [Package.name|depends-package|]
                        , [Package.name|depmissing-package|]
                        , [Package.name|providers-1|]
                        , [Package.name|providers-2|]
                        , [Package.name|test-package|]
                        ]
                    )
            db <- registerSyncdb h "testdb" []
            pkgs1 <- dbGetPkgcache db >>= mapM pkgGetName
            dbAddServer h db (getSyncdbUrl [reldir|testdb|])
            dbUpdate h [db] False `shouldReturn` DbUpdated
            pkgs2 <- dbGetPkgcache db >>= mapM pkgGetName
            (pkgs1, pkgs2) `shouldBe` ref

        testAlpm "Find package" True $ \_getPackageFile _getSyncdbUrl h -> do
            localDb <- getLocaldb h
            mpkg <- findDbsSatisfier h [localDb] "test-package"
            mpkg' <- traverse pkgGetName mpkg
            mpkg' `shouldBe` Just [Package.name|test-package|]

        describe "Install package to local db" $ do
            testAlpm "From file" False $ \getPackageFile _getSyncdbUrl h -> do
                let
                    ref = ([], [[Package.name|test-package|]])

                localDb <- getLocaldb h
                pkgs1 <- dbGetPkgcache localDb >>= mapM pkgGetName

                pkg <-
                    pkgLoad
                        h
                        False
                        []
                        (getPackageFile [relfile|test-package/test-package-1-1-any.pkg.tar.zst|])
                withTrans h [] $ do
                    addPkg h pkg
                    transPrepare h
                    transCommit h

                pkgs2 <- dbGetPkgcache localDb >>= mapM pkgGetName

                (pkgs1, pkgs2) `shouldBe` ref

            testAlpm "From sync db" False $ \_getPackageFile getSyncdbUrl h -> do
                let
                    ref = ([], [[Package.name|test-package|]])

                localDb <- getLocaldb h
                pkgs1 <- dbGetPkgcache localDb >>= mapM pkgGetName

                db <- registerSyncdb h "testdb" []
                dbAddServer h db (getSyncdbUrl [reldir|testdb|])
                dbUpdate h [db] False `shouldReturn` DbUpdated
                Just pkg <- dbGetPkg db [Package.name|test-package|]
                withTrans h [] $ do
                    addPkg h pkg
                    transPrepare h
                    transCommit h

                pkgs2 <- dbGetPkgcache localDb >>= mapM pkgGetName

                (pkgs1, pkgs2) `shouldBe` ref

            testAlpm "With missing dependencies" False $ \_getPackageFile getSyncdbUrl h -> do
                let
                    ref :: AlpmError AlpmTransactionError
                    ref =
                        AlpmError AlpmErrUnsatisfiedDeps
                            $ TransPrepareDepmissingError
                                [ AlpmDepmissing
                                    "depmissing-package"
                                    (AlpmDepend [Package.name|missing-package|] ConstraintAny)
                                    Nothing
                                ]

                db <- registerSyncdb h "testdb" []
                dbAddServer h db (getSyncdbUrl [reldir|testdb|])
                dbUpdate h [db] False `shouldReturn` DbUpdated
                Just pkg <- dbGetPkg db [Package.name|depmissing-package|]
                withTrans h [] $ do
                    addPkg h pkg
                    transPrepare h `shouldThrow` (== ref)

testAlpm
    :: Text
    -> Bool
    -> ( (Path Rel File -> FilePath)
         -> (Path Rel Dir -> String)
         -> AlpmHandlePtr
         -> Expectation
       )
    -> Spec
testAlpm name setupDirectories action = do
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

        getPackageFile :: Path Rel File -> FilePath
        getPackageFile path =
            fromAbsFile (dataDir </> [reldir|packages|] </> path)

        getSyncdbUrl :: Path Rel Dir -> String
        getSyncdbUrl db =
            "file://" <> fromAbsDir (dataDir </> [reldir|syncdbs|] </> db)

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
    before_ setup . it (Text.unpack name) $ do
        withAlpm
            (fromAbsDir rootDir)
            (fromAbsDir databaseDir)
            (action getPackageFile getSyncdbUrl)
