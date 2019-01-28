#!/usr/bin/env bash

set -xe

nix-shell --pure --run 'hoogle server --local' ./hoogle.nix

