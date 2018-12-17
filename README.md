# prana

प्राण, prāṇa; the Sanskrit word for "life force" or "vital principle"
constructed from pra meaning movement and an meaning constant.

## Setup

Build a docker image with a patched GHC that outputs .prana files:

    $ sh scripts/buildimage.sh

Copy the compiled standard libraries (ghc-prim, integer-gmp and base):

    $ sh scripts/copylibs.sh

Run the demo:

    $ sh scripts/compiledemo.sh
