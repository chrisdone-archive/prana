# prana [experiment; WIP]

An interpreter for GHC Haskell programs

## Name

प्राण, prāṇa; the Sanskrit word for "life force" or "vital principle"
constructed from pra meaning movement and an meaning constant.

## Setup

Build a docker image with a patched GHC that outputs .prana files:

    $ sh scripts/buildimage.sh

Copy the compiled standard libraries (ghc-prim, integer-gmp and base):

    $ sh scripts/copylibs.sh

Run the demo:

    $ sh scripts/compiledemo.sh

## Architecture

How it works:

* GHC is patched to output .prana files along with .hi and .o files,
  which contain ASTs of the GHC Core for each module along with other
  metadata.
* Prana reads all these files in on start-up and interprets any
  expression or top-level binding desired.
* Prana itself is written in GHC Haskell, so it can re-use GHC's own
  runtime to implement primitive operations.
