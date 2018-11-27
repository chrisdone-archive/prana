rm -f *.prana
docker run -v`pwd`:`pwd` -w`pwd` --rm ghc-compile ghc Demo.hs -O0 -fbyte-code && stack build && stack exec -- prana $(find . -name '*.prana') +RTS -s
