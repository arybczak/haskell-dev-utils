#!/bin/bash

cabal install                             \
  -j                                      \
  --ghc-options="-j +RTS -A64m -n2m -RTS" \
  --enable-executable-dynamic             \
  --dependencies-only                     \
  "$@"
