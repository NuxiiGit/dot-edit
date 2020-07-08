#!/bin/bash
ghc -o ${1-bin/hay-star} -odir bin/obj -hidir bin/interface -ilib_core -ilib_dot --make src/Main.hs
