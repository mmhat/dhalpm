let testdir = "test/.out/install-from-file"
in
{ root-dir = "${testdir}/root"
, database-dir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "test-package"
    , sigcheck = SiglevelCheck/Type.CheckNever
    , sigtrust = SiglevelTrust/Type.TrustAll
    , build = Some
      { path = "test/databases/testdb/test-package-1-1-any.pkg.tar.zst"
      , script = ""
      }
    }
  ]
}
