\(data-dir : Text) ->
\(test-dir : Text) ->
  { root-dir = "${test-dir}/root"
  , database-dir = "${test-dir}/database"
  , packages =
    [ Package::{
      , name = "filesystem"
      , sigcheck = SiglevelCheck/Type.CheckNever
      , sigtrust = SiglevelTrust/Type.TrustAll
      , databases =
        [ { name = "core"
          , sigcheck = SiglevelCheck/Type.CheckNever
          , sigtrust = SiglevelTrust/Type.TrustAll
          , servers = ./test/data/mirrorlist.dhall "core" "x86_64"
          }
        ]
      }
    ]
  }
