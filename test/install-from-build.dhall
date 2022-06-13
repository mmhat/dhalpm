let testdir = "test/.out/install-from-build"
let srcdir = "${env:PWD as Text}/test/packages/test-package"
let builddir = "${env:PWD as Text}/${testdir}/test-package"
in
{ rootDir = "${testdir}/root"
, databaseDir = "${testdir}/db"
, packages =
  [ Package ::
    { name = "test-package"
    , sigcheck = SiglevelCheckT.CheckNever
    , sigtrust = SiglevelTrustT.TrustAll
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
