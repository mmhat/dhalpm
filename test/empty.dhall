let testdir = "test/.out/empty"
in
{ rootDir = "${testdir}/root"
, databaseDir = "${testdir}/db"
, packages = ([] : List PackageT)
}
