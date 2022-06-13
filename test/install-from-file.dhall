let testdir = "test/.out/install-from-file"
in
{ rootDir = "${testdir}/root"
, databaseDir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "test-package"
    , sigcheck = SiglevelCheckT.CheckNever
    , sigtrust = SiglevelTrustT.TrustAll
    , build = Some
      { path = "test/databases/testdb/test-package-1-1-any.pkg.tar.zst"
      , script = ""
      }
    }
  ]
}
