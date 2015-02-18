#!/bin/bash

PATH="./.cabal-sandbox/bin:$PATH"

doctest -isrc -Wall -fno-warn-type-defaults "$@"
