#!/bin/bash

BASEDIR="$PWD"
TARGETDIR="$1"

cd "$TARGETDIR"

PKGNAME="$(cabal info . | awk '/^\*/{print $2}')"
SRCTGZ="${PKGNAME}.tar.gz"
DIST="$(stack path | awk '/^dist-dir/ {print $2}')"

if [ -f "${DIST%/}/$SRCTGZ" ]; then
    cd "$DIST"
    tar xvzf "$SRCTGZ"
    cd "$BASEDIR/$TARGETDIR"
    NG=$(git ls-tree -r HEAD | while read perm blob hash name; do [ -e "$DIST/$PKGNAME/$name" ] || echo "$name"; done)
    if [ -n "$NG" ]; then
        echo "Missing files:"
        echo $NG
        exit 1
    fi
fi
