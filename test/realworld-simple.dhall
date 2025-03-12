let testdir = "test/.out/realworld-simple"
in
{ root-dir = "${testdir}/root"
, database-dir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "filesystem"
    , sigcheck = SiglevelCheck/Type.CheckNever
    , sigtrust = SiglevelTrust/Type.TrustAll
    , databases =
      [ { name = "core"
        , sigcheck = SiglevelCheck/Type.CheckNever
        , sigtrust = SiglevelTrust/Type.TrustAll
        , servers = ./mirrorlist.dhall "core" "x86_64"
        }
      ]
    }
  ]
}
