#!/bin/bash

ghc -O2 -prof -fprof-auto -rtsopts Main.hs ./Network/tuntap.o -threaded
