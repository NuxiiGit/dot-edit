#!/bin/bash
ghc -o bin/app -odir bin/obj -hidir bin/interface -ilib_core -ilib_dot --make src/Main.hs
