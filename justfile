# shellcheck shell=bash

LC_ALL := 'C.UTF-8'

default:
  @just --list

# check that the code is properly linted
hlint:
  nix develop --command hlint .

# build the docker image
build-docker:
  nix build .
  mkdir -p tmp
  chmod +w -R tmp
  rm -rf ./tmp/*
  # shellcheck disable=SC2046
  cp -R $(nix-store -qR result/) tmp
  cp -L ./result/bin/cardano-address ./tmp/cardano-address
  docker build . -t cardano-address

# build after clean
clean-build-docker:
  cabal clean
  just build-docker
