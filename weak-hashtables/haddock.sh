#!/bin/sh

set -x

rm -Rf dist/doc

HADDOCK_OPTS='--html-location=http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

cabal haddock $HADDOCK_OPTS --hyperlink-source $@
