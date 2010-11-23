#!/bin/sh
#
# Commentary:
#
# Check the distribution for errors
#
# Will catch missing distfiles and build-errors due to missing modules.

set -e
#set -x

# Cabal package lint
cabal check >/dev/null
if [ $? -ne 0 ] ; then
    echo "checkdist.sh: 'cabal check' failed" >&2
    exit 1
fi

# Create source distribution
[ *.cabal -nt dist/setup-config ] && cabal configure -v0
cabal sdist -v0
if [ $? -ne 0 ] ; then
    echo "checkdist.sh: 'cabal sdist' failed" >&2
    exit 1
fi

# Unpack source distribution and attempt to build it
distname=`awk '/^name: / { print $2 }' xine.cabal`
distver=`awk '/^version: / { print $2 }' xine.cabal`
distsrc="./dist/${distname}-${distver}.tar.gz"

tmpdir=`mktemp -d`
tar xf "$distsrc" -C "$tmpdir"
cd "${tmpdir}/${distname}-${distver}"
cabal configure -v0 -O0 --ghc-options=-Werror && cabal build -v0
if [ $? -ne 0 ] ; then
    rm -rf "$tmpdir"
    echo "checkdist.sh: 'cabal build' failed; temporary files in $tmpdir" >&2
    exit 1
fi
cabal haddock -v0
if [ $? -ne 0 ] ; then
    rm -rf "$tmpdir"
    echo "checkdist.sh: 'cabal haddock' failed" >&2
    exit 1
fi

rm -rf "$tmpdir"
exit 0
