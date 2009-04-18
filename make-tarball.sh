#!/bin/bash

# simple tarball creator

tar --verbose --create --file hbejeweler.tar.gz --gzip \
	README \
	build.sh \
	*.hs \
	game-tree-0.1.0.0

