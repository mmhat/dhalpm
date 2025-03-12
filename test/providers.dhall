let testdir = "test/.out/providers"
in
{ root-dir = "${testdir}/root"
, database-dir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "providers"
    , sigcheck = SiglevelCheck/Type.CheckNever
    , sigtrust = SiglevelTrust/Type.TrustAll
    , providers = ["providers-2"]
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
