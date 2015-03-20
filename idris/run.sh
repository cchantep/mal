#! /bin/sh

DIR=`dirname "$0"`
idris --build "$DIR/mal.ipkg" && "$DIR/mal"
