#!/usr/bin/env bash

set -eu
shopt -s extglob

if [[ ! -f cabal.project ]]; then
    echo "Should be run from project root" >&2
    exit 1
fi

pushd utils > /dev/null

setup_test_database="${PWD}/setup-test-database"

gcc -lalpm -o "${setup_test_database}" setup-test-database.c

popd > /dev/null

pushd test-data > /dev/null

{
    declare -g -i found_server=0
    echo '\(repo : Text) -> \(arch : Text) -> ['
    curl -Ls 'https://archlinux.org/mirrorlist/all/https/' | while read -r line; do
        case "${line}" in
            *([[:space:]])*([#])*([[:space:]])Server*([[:space:]])=*)
                found_server+=1
                echo "${line}"
                ;;
            *)
                if (( ${found_server} )); then
                    break
                fi
                ;;
        esac
    done | sed -e 's|^#*Server *= *|, "|' -e 's|$|"|' -e 's|$repo|${repo}|' -e 's|$arch|${arch}|'
    echo ']'
} | dhall format | tee mirrorlist.dhall

makepkg_conf="${PWD}/makepkg.conf"

for pkg in packages/*; do
    pushd "${pkg}" > /dev/null
    rm ./*.pkg.tar.zst
    makepkg --config "${makepkg_conf}" --nodeps --clean
    popd > /dev/null
done

rm -rf root database
mkdir -p root database
"${setup_test_database}" root database \
    packages/depends-package/*.pkg.tar.zst \
    packages/test-package/*.pkg.tar.zst

rm -rf syncdbs
mkdir -p syncdbs/testdb

cp packages/*/*.pkg.tar.zst syncdbs/testdb
pushd syncdbs/testdb > /dev/null
repo-add testdb.db.tar.gz ./*.pkg.tar.zst
popd > /dev/null

assets=(mirrorlist.dhall makepkg.conf packages/*/*.pkg.tar.zst packages/*/PKGBUILD root database syncdbs)

popd > /dev/null

for package in alpm dhalpm; do
    data_directory="${package}/test/data"
    rm -rf "${data_directory}"
    mkdir -p "${data_directory}"
    for asset in "${assets[@]}"; do
        source_path="test-data/${asset}"
        target_path="${data_directory}/${asset}"
        mkdir -p "$(dirname "${target_path}")"
        cp --recursive "${source_path}" "${target_path}"
    done
done
