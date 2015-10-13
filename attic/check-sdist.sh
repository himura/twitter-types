#!/bin/bash

BASEDIR="$PWD"
TARGETDIR="$1"
DIST="$(stack path | awk '/^dist-dir/ {print $2}')"

cd "$TARGETDIR"

if [ -f ${DIST%/}/${TARGETDIR}-*.tar.gz ]; then
    cd "$DIST"
    srctgz=(${TARGETDIR}*.tar.gz)
    srctgz="${srctgz[0]}"
    pkgname="${srctgz%.tar.gz}"
    tar xvzf "${srctgz}"
    cd "$BASEDIR/$TARGETDIR"
    NG=$(git ls-tree -r HEAD | while read perm blob hash name; do [ -e "$DIST/$pkgname/$name" ] || echo "$name"; done)
    if [ -n "$NG" ]; then
        echo "Missing files:"
        echo $NG
        exit 1
    fi
fi
