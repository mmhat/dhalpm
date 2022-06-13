#!/usr/bin/bash

set -eux

cur="${PWD}"
makepkg_conf="${cur}/test/makepkg.conf"

for pkg in test/packages/*; do
    pushd "${pkg}"
    makepkg --config "${makepkg_conf}" -d -f
    popd
done

sudo rm -rf "${cur}/test/databases/localdb"
sudo mkdir -p "${cur}/test/databases/localdb"/{db,root}
sudo pacman --noconfirm -U \
    --dbpath "${cur}/test/databases/localdb/db" \
    --root "${cur}/test/databases/localdb/root" \
    "${cur}/test/packages/depends-package/depends-package-1-1-any.pkg.tar.zst" \
    "${cur}/test/packages/test-package/test-package-1-1-any.pkg.tar.zst"
sudo chown -R "$UID:$GROUPS" "${cur}/test/databases/localdb"

rm -rf "${cur}/test/databases/testdb"
mkdir -p "${cur}/test/databases/testdb"
cp "${cur}/test/packages"/*/*.pkg.tar.zst "${cur}/test/databases/testdb"
pushd "${cur}/test/databases/testdb"
repo-add testdb.db.tar.gz *.pkg.tar.zst
