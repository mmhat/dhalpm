{-# LANGUAGE QuasiQuotes #-}

module Archlinux.AlpmSpec where

import RIO

import Foreign hiding (void)
import Path
import Path.IO
import qualified RIO.Directory as D
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import Test.Hspec
import Test.QuickCheck

import Archlinux.Alpm



spec :: Spec
spec = do
    -- TODO: Parsing package names
    -- TODO: Parsing package versions

    describe "alpm_list_t" $ do
        it "fromAlpmList . toAlpmList" $
            property $ \lst -> withCStrings lst $ \lst' -> do
                res <- withAlpmList lst' fromAlpmList
                res `shouldBe` lst'

    describe "Alpm Dependencies" $ do
        it "peek . poke" $ do
            let ref = AlpmDepend
                    { alpmDependName = "pkg"
                    , alpmDependConstraint = ConstraintAny
                    }
            res <- alloca $ \p -> do
                poke p ref
                peek p
            res `shouldBe` ref

        it "depComputeString . depFromString" $ do
            let ref = "pkg"
            p <- depFromString ref
            res <- depComputeString p
            depFree p
            res `shouldBe` ref

        it "Compute depstring from AlpmDepend" $ do
            let x = AlpmDepend
                    { alpmDependName = "pkg"
                    , alpmDependConstraint = ConstraintAny
                    }
                ref = "pkg"
            res <- alloca $ \p -> do
                poke p x
                depComputeString p
            res `shouldBe` ref

    describe "Alpm databases" $ do
        testAlpm "Local db name" $ \h -> do
            let ref = "local"
            db <- getLocaldb h
            dbGetName db `shouldReturn` ref

        testAlpm "Sync db name" $ \h -> do
            let ref = "testdb"
            db <- registerSyncdb h "testdb" []
            dbGetName db `shouldReturn` ref

        testAlpm "Double register sync db" $ \h -> do
            let ref = ["testdb"]
            void $ registerSyncdb h "testdb" []
            void $ registerSyncdb h "testdb" []
            (getSyncdbs h >>= mapM dbGetName) `shouldReturn` ref

        testAlpm "Register sync db ordering" $ \h -> do
            let ref = ["testdb", "testdb2", "testdb3"]
            void $ registerSyncdb h "testdb"  []
            void $ registerSyncdb h "testdb2" []
            void $ registerSyncdb h "testdb"  []
            void $ registerSyncdb h "testdb3" []
            (getSyncdbs h >>= mapM dbGetName) `shouldReturn` ref

        testAlpm "Update sync db" $ \h -> do
            let ref = ([], ["depends-package", "depmissing-package", "providers-1", "providers-2", "test-package"])
            cur <- D.getCurrentDirectory
            let dbUri = "file://" ++ cur ++ "/test/databases/testdb"
            db <- registerSyncdb h "testdb" []
            pkgs1 <- dbGetPkgcache db >>= mapM pkgGetName
            dbAddServer h db dbUri
            dbUpdate h [db] False `shouldReturn` DbUpdated
            pkgs2 <- dbGetPkgcache db >>= mapM pkgGetName
            (pkgs1, pkgs2) `shouldBe` ref

        testAlpmWithDatabase "Find package" [reldir|localdb|] $ \h -> do
            localDb <- getLocaldb h
            mpkg <- findDbsSatisfier h [localDb] "test-package"
            mpkg' <- traverse pkgGetName mpkg
            mpkg' `shouldBe` Just "test-package"

        describe "Install package to local db" $ do
            testAlpm "From file" $ \h -> do
                let ref = ([], ["test-package"])

                localDb <- getLocaldb h
                pkgs1 <- dbGetPkgcache localDb >>= mapM pkgGetName

                pkg <- pkgLoad h False [] "test/databases/testdb/test-package-1-1-any.pkg.tar.zst"
                withTrans h [] $ do
                    addPkg h pkg
                    transPrepare h
                    transCommit h

                pkgs2 <- dbGetPkgcache localDb >>= mapM pkgGetName

                (pkgs1, pkgs2) `shouldBe` ref

            testAlpm "From sync db" $ \h -> do
                let ref = ([], ["test-package"])

                localDb <- getLocaldb h
                pkgs1 <- dbGetPkgcache localDb >>= mapM pkgGetName

                cur <- D.getCurrentDirectory
                let dbUri = "file://" ++ cur ++ "/test/databases/testdb"
                db <- registerSyncdb h "testdb" []
                dbAddServer h db dbUri
                dbUpdate h [db] False `shouldReturn` DbUpdated
                pkg <- dbGetPkg db "test-package"
                withTrans h [] $ do
                    addPkg h pkg
                    transPrepare h
                    transCommit h

                pkgs2 <- dbGetPkgcache localDb >>= mapM pkgGetName

                (pkgs1, pkgs2) `shouldBe` ref

            testAlpm "With missing dependencies" $ \h -> do
                let ref :: AlpmError AlpmTransactionError
                    ref = AlpmError AlpmErrUnsatisfiedDeps $ TransPrepareDepmissingError [AlpmDepmissing "depmissing-package" (AlpmDepend "missing-package" ConstraintAny) Nothing]

                cur <- D.getCurrentDirectory
                let dbUri = "file://" ++ cur ++ "/test/databases/testdb"
                db <- registerSyncdb h "testdb" []
                dbAddServer h db dbUri
                dbUpdate h [db] False `shouldReturn` DbUpdated
                pkg <- dbGetPkg db "depmissing-package"
                withTrans h [] $ do
                    addPkg h pkg
                    transPrepare h `shouldThrow` (== ref)

testAlpm :: Text -> (AlpmHandlePtr -> Expectation) -> Spec
testAlpm n k = do
    let baseDir = [reldir|test/.out/alpm|]
    testFn <- runIO $ parseRelDir $ T.unpack $ T.replace " " "_" $ T.toLower n
    let dir = baseDir </> testFn
        dbdir = dir </> [reldir|db|]
        rootdir = dir </> [reldir|root|]
        setup = do
            whenM (doesDirExist dir) $
                removeDirRecur dir
            ensureDir dbdir
            ensureDir rootdir
    before_ setup $ it (T.unpack n) $
        withAlpm (fromRelDir rootdir) (fromRelDir dbdir) k

testAlpmWithDatabase :: Text -> Path Rel Dir -> (AlpmHandlePtr -> Expectation) -> Spec
testAlpmWithDatabase n tplDir k = do
    let baseDir = [reldir|test/.out/alpm|]
        dataDir = [reldir|test/databases|]
    testFn <- runIO $ parseRelDir $ T.unpack $ T.replace " " "_" $ T.toLower n
    let dir = baseDir </> testFn
        dbdir = dir </> [reldir|db|]
        rootdir = dir </> [reldir|root|]
        setup = do
            whenM (doesDirExist dir) $
                removeDirRecur dir
            copyDirRecur (dataDir </> tplDir) dir
            ensureDir dbdir
            ensureDir rootdir
    before_ setup $ it (T.unpack n) $
        withAlpm (fromRelDir rootdir) (fromRelDir dbdir) k
