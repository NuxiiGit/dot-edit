@echo off
set alias=%1
:: check whether there was an argument
if "%alias%" == "" (
    set alias="app"
)
:: compile
ghc -o %alias% ^
        -idirs;src;app;lib ^
        -odir bin/obj ^
        -hidir bin/interface ^
        --make ^
        app/Main.hs
