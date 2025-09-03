#!/bin/sh

set -o errexit

ghcup install ghc latest --set

cabal update
cabal test --flag ci
