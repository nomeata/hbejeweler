#!/bin/bash

# simple build script for hbejeweler

ghc --make -O2 -igame-tree-0.1.0.0/src Main.hs -o hbejeweler
