let testdir = "test/.out/install-from-build"
let srcdir = "${env:PWD as Text}/test/packages/test-package"
let builddir = "${env:PWD as Text}/${testdir}/test-package"
in
{ root-dir = "${testdir}/root"
, database-dir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "test-package"
    , sigcheck = SiglevelCheck/Type.CheckNever
    , sigtrust = SiglevelTrust/Type.TrustAll
    , build = Some
      { path = "${builddir}/test-package-1-1-any.pkg.tar.zst"
      , script = ''
        rm -rf '${builddir}'
        cp -r '${srcdir}' '${builddir}'
        cd '${builddir}'
        makepkg --config '${env:PWD as Text}/test/makepkg.conf' -f > /dev/null
        ''
      }
    }
  ]
}
