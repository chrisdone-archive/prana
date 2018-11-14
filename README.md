# prana

# GHC 8.0

Generate ghc-prim, integer-simple and base:

    docker build . -f Dockerfile.ghc-8.0 -t ghc-compile
    docker run -v`pwd`:`pwd` -w`pwd` --rm ghc-compile cp /root/ghc_build/ghc-8.0/libraries/libraries.tar.gz .
    mkdir libraries-8.0/
    cd libraries-8.0/
    tar xf ../libraries.tar.gz
    cd ..
    rm libraries.tar.gz

Run GHC compiler example:

    docker run -v`pwd`:`pwd` -w`pwd` --rm ghc-compile ghc Demo.hs && sudo chown chris:chris main_Demo.prana
