\(data-dir : Text) ->
\(test-dir : Text) ->
  { root-dir = "${test-dir}/root"
  , database-dir = "${test-dir}/database"
  , packages =
    [ Package::{
      , name = "test-package"
      , sigcheck = SiglevelCheck/Type.CheckNever
      , sigtrust = SiglevelTrust/Type.TrustAll
      , build = Some
        { path =
            "${data-dir}/packages/test-package/test-package-1-1-any.pkg.tar.zst"
        , script = ""
        }
      }
    ]
  }
