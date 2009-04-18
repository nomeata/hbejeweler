#!/bin/bash

# simple tarball creator

cd ..
test -d hbejeweler || { echo did not find hbejeweler; exit 1; }
tar --verbose --create --file hbejeweler/hbejeweler.tar.gz --gzip \
	hbejeweler/README \
	hbejeweler/build.sh \
	hbejeweler/*.hs \
	hbejeweler/game-tree-0.1.0.0

