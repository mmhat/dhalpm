let testdir = "test/.out/remove-orphaned"
in
{ rootDir = "${testdir}/root"
, databaseDir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "test-package"
    , sigcheck = SiglevelCheckT.CheckNever
    , sigtrust = SiglevelTrustT.TrustAll
    , databases =
      [ { name = "testdb"
        , sigcheck = SiglevelCheckT.CheckNever
        , sigtrust = SiglevelTrustT.TrustAll
        , servers =
          [ "file://${env:PWD as Text}/test/databases/testdb"
          ]
        }
      ]
    }
  ]
}
