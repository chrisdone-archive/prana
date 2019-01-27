# prana [experiment; WIP]

An interpreter for GHC Haskell programs

## Name

प्राण, prāṇa; the Sanskrit word for "life force" or "vital principle"
constructed from pra meaning movement and an meaning constant.

## Implementation challenges

The interpreter isn't written yet (I wrote a prototype
[here](https://github.com/chrisdone/prana/blob/4926074322df23568866061f2c036915f06fa122/src/Prana/Interpret.hs)
and deleted it), I'm still working on producing a pristine Core output
from GHC for Prana to consume and interpret efficiently.

Challenges and thoughts:

* ~~Names of Core identifiers are not properly globally unique. GHC's
  Unique is per run of GHC, not global across runs. Make a process
  that normalizes all these names into a monotonically increasing
  integer. Then have a separate mapping from Int to ByteString with a
  human-friendly description of the binding.~~

  **Done.** There is now a `names.txt` file that contains a binary
  encoded list of exported names and a list of local names. Their
  index in the list determines what will be put into the
  AST.

  E.g. `VarE (ExportedIndex 123)` represents the 123rd global
  variable in `names.txt`. This can be used for the `Vector Exp`-type
  of data structure described in a later bullet point.

  Meanwhile, `VarE (LocalIndex 5678)` means "a globally unique
  identifier" that is declared and used within a local module and
  therefore its value has to be looked up from a dynamic environment
  of bound things via let/lambdas.

* Test suite is up and running
  [here](https://github.com/chrisdone/prana/blob/22f8bdfa9dff860e306d6bca8f6dbdaffc864d76/test/Main.hs#L27),
  this demonstrates the LocalId vs ExportedId difference.

* Implement LET and LAMBDA using an environment, rather than
  beta-substitution (as in the
  [old interpreter](https://github.com/chrisdone/prana/blob/4926074322df23568866061f2c036915f06fa122/src/Prana/Interpret.hs)). Beta-substitution
  requires reconstructing a fresh tree, which is not efficient.

* Once names really are unique, we don't need to do lookups on
  strings, we can instead normalize the numbers to allow O(1) lookup
  in a vector (`Vector Exp`) for globals. For locals, I think we can
  just use a vector (`Vector (Int,Exp)`) and do O(n) lookup; on a
  small enough vector it'll fit in cache and O(n) Int64 comparisons
  over 10 elements is fast.

  In practice I think the data structure for globals will be:
  `Unboxed.Vector (Int, Int)` where they map to (index, offset) that
  that can let you find a view on an `Exp` via `S.take offset (S.drop
  index bs)`.

* All methods existing in their own namespace, and are indices into
  the right slot of the dictionary. So in the database, we should
  store methodname+slot_number.

* A `DictId` is in its own namespace, should yield a dictionary as an
  array of `Exp` methods. When a method is called, it indexes on the
  array.

* Ignoring type applications for which functions don't actually even
  accept an argument for that type. Except tagToEnum _does_ expect a
  type argument. Core is incoherent that way. We should normalize it
  before writing byte-code to disk.

* Instead of decoding the AST with the `binary` package, use a
  PatternSynonym (as demonstrated in `Prana.View`) to simply walk the
  AST in a read-only fashion, with no new construction of AST
  nodes. This would (ideally) allow keeping the AST in CPU cache,
  avoiding mainline memory accesses, leading to nice speeds.

* Consider use of unboxed sums for the WHNF data type.

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

## File sizes of compiled byte-code

Output here:

https://gist.github.com/chrisdone/5ed9adf9dba5fd82d582e9f2bbc30c9f
