let testdir = "test/.out/realworld-simple"
in
{ rootDir = "${testdir}/root"
, databaseDir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "filesystem"
    , sigcheck = SiglevelCheckT.CheckNever
    , sigtrust = SiglevelTrustT.TrustAll
    , databases =
      [ { name = "core"
        , sigcheck = SiglevelCheckT.CheckNever
        , sigtrust = SiglevelTrustT.TrustAll
        , servers = ./mirrorlist.dhall "core" "x86_64"
        }
      ]
    }
  ]
}
