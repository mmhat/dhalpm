let Text/shell-escape =
      https://prelude.dhall-lang.org/v23.1.0/Text/shell-escape.dhall
        sha256:d53521b3f478cb18a7d63730bc0e3153c2124b70d6ac9a1f610ce7db67cfc7a2

in  \(data-dir : Text) ->
    \(test-dir : Text) ->
      let src-dir = Text/shell-escape "${data-dir}/packages/test-package"

      let output-dir = "${test-dir}/test-package"

      let build-dir = Text/shell-escape output-dir

      let makepkg = Text/shell-escape "${data-dir}/makepkg.conf"

      in  { root-dir = "${test-dir}/root"
          , database-dir = "${test-dir}/database"
          , packages =
            [ Package::{
              , name = "test-package"
              , sigcheck = SiglevelCheck/Type.CheckNever
              , sigtrust = SiglevelTrust/Type.TrustAll
              , build = Some
                { path = "${output-dir}/test-package-1-1-any.pkg.tar.zst"
                , script =
                    ''
                    rm -rf ${build-dir}
                    mkdir -p ${build-dir}
                    cp ${src-dir}/PKGBUILD ${build-dir}/PKGBUILD
                    cd ${build-dir}
                    makepkg --config ${makepkg} > /dev/null
                    ''
                }
              }
            ]
          }
