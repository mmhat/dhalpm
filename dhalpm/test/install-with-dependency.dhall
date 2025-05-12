\(data-dir : Text) ->
\(test-dir : Text) ->
  { root-dir = "${test-dir}/root"
  , database-dir = "${test-dir}/database"
  , packages =
    [ Package::{
      , name = "depends-package"
      , sigcheck = SiglevelCheck/Type.CheckNever
      , sigtrust = SiglevelTrust/Type.TrustAll
      , databases =
        [ { name = "testdb"
          , sigcheck = SiglevelCheck/Type.CheckNever
          , sigtrust = SiglevelTrust/Type.TrustAll
          , servers = [ "file://${data-dir}/syncdbs/testdb" ]
          }
        ]
      }
    ]
  }
