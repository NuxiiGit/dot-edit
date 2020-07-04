@echo off
set alias="%1"
:: check whether there was an argument
if %alias% == "" (
    set alias="app"
)
:: compile
ghc -o %alias% --odir bin/obj -hidir bin/interface -ilib_core -ilib_dot --make src/Main.hs
