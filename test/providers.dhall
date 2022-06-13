let testdir = "test/.out/providers"
in
{ rootDir = "${testdir}/root"
, databaseDir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "providers"
    , sigcheck = SiglevelCheckT.CheckNever
    , sigtrust = SiglevelTrustT.TrustAll
    , providers = ["providers-2"]
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
