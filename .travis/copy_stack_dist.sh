#!/bin/bash

# copy mix and tix files to ordinal path to support hpc-coveralls

STACK_DIST_DIR=twitter-types/$HASKELL_DIST_DIR
HPC_DIR=twitter-types/dist/hpc/vanilla

HPC_MIX_DIR=$HPC_DIR/mix/twitter-types-$(awk '/^version:/{print $2}' twitter-types/twitter-types.cabal)/

mkdir -p $HPC_DIR

mkdir -p $HPC_DIR/tix/tests
cp $STACK_DIST_DIR/hpc/tests.tix $HPC_DIR/tix/tests

mkdir -p $HPC_MIX_DIR
cp -a $STACK_DIST_DIR/hpc/.hpc/* $HPC_MIX_DIR/


