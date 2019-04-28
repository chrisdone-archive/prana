# prana-types

This package provides all the types neccessary for compiling Haskell
code to prana byte code and for loading it ready for the interpreter.

Broadly speaking, it contains:

1. An STG AST set of data types, which correspond almost 1:1 with
   GHC's, but with minor modifications to make it more strict, and to
   not depend on the GHC API directly.
2. The name index types, which serve to map integers used in the AST
   to their original names, and a reverse index to go in the other
   direction.
