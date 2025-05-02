let testdir = "test/.out/install-from-syncdb"
in
{ root-dir = "${testdir}/root"
, database-dir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "test-package"
    , sigcheck = SiglevelCheck/Type.CheckNever
    , sigtrust = SiglevelTrust/Type.TrustAll
    , databases =
      [ { name = "testdb"
        , sigcheck = SiglevelCheck/Type.CheckNever
        , sigtrust = SiglevelTrust/Type.TrustAll
        , servers =
          [ "file://${env:PWD as Text}/test/databases/testdb"
          ]
        }
      ]
    }
  ]
}
