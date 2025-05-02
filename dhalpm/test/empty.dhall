let testdir = "test/.out/empty"
in
{ root-dir = "${testdir}/root"
, database-dir = "${testdir}/db"
, packages = ([] : List Package/Type)
}
