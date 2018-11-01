# prana

## Build GHC's base packages

Use docker to download & compile GHC and generate the base package
.prana files.

    $ docker build . -f prana-base-packages/Dockerfile.ghc -t ghc-compile

## Build the compiler packages

Make sure you already built the base packages as above. Now you can
build the compiler.

    $ stack build
