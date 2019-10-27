# prana

An interpreter for GHC STG written in Haskell itself

:construction: Prana is under construction.

See
[test/assets/](https://github.com/chrisdone/prana/tree/master/prana-interpreter/test/assets)
under `prana-interpreter` for example programs that run with prana.

[WIP slides on Prana](https://chrisdone.com/pdfs/prana.pdf).

## Milestones

#### Pure-Haskell goals

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Clean STG from GHC|  :heavy_check_mark: | Laborious |
|Interpreter prototype for AST| :heavy_check_mark: | Fairly easy |
|Basic test suite for interpreter| :heavy_check_mark: | Easy |
|Move interpreter into `prana-interpret` library| :heavy_check_mark: | Easy |
|Support all primitive types (`Char#` etc.)| :heavy_check_mark: | Fairly straight-forward |
|`tagToEnum` support| :heavy_check_mark: | Pain in the arse |
|Report unimplemented primops when compiling| :heavy_check_mark: | Straight-forward |
|Primops for register types (`Word#`, `Int#`, etc.)| :heavy_check_mark: | Straight-forward |
|Primops for arrays (`ByteArray#`, etc.)| :heavy_check_mark: | Fairly straight-forward |
|Have a sophisticated inspector| – | Fairly straight-forward |
|Get code coverage to near 100%| – | Detailed |
|Code cleanup| – | Straight-forward |

#### GHC Haskell goals

|Milestone|Status|Appraisal|
|:---|:---|:---|
|FFI support| :construction: | Requires learning but should design itself |
|IO primops| :heavy_check_mark: | Fairly straight-forward |
|Runtime exceptions (`raise#`)| – | Tricky, space for UX design |
|Run [base package tests](https://github.com/ghc/packages-base/tree/master/tests) | – | Fairly straight-forward |
|Add hotswapping support (like Emacs)| – | Requires exploration/experiment |
|Threaded runtime/concurrency| – | Could be tricky |

##### FFI support

* All the base packages only use static FFI calls, we can ignore
  dynamic ones for now.
* Wrappers are used in the base packages.
* The following prim calls (custom primops) are used:
  * PrimCall "stg_word32ToFloatzh" base
  * PrimCall "stg_floatToWord32zh" base
  * PrimCall "stg_word64ToDoublezh" base
  * PrimCall "stg_doubleToWord64zh" base

Meanwhile, objectDir from DynFlags contains

```
chris@precision:~/Work/chrisdone/prana$ ls -alh ghc-8.4/libraries/ghc-prim/dist/build
total 14M
drwxr-xr-x 5 chris chris 4.0K May 25 11:54 .
drwxr-xr-x 4 chris chris 4.0K May 25 11:51 ..
drwxr-xr-x 2 chris chris 4.0K May 25 11:47 autogen
drwxrwxr-x 2 chris chris 4.0K Feb 27 18:13 cbits
drwxrwxr-x 2 chris chris 4.0K May 25 11:43 GHC
-rw-rw-r-- 1 chris chris 7.0M May 25 11:41 libHSghc-prim-0.5.2.0-Bfo9y0qb0emG5VRfx5d4mv.a
-rwxrwxr-x 1 chris chris 6.9M May 25 11:51 libHSghc-prim-0.5.2.0-Bfo9y0qb0emG5VRfx5d4mv-ghc8.4.3.so
```

Which is interesting for two use-cases:

1. We could copy the whole .so file alongside our .prana file, and
   then simply load it up with dlopen() and lookup the C functions.
2. Or, alternatively, we could just link together all the cbits (see
   below) into one ghc-prim.so, along with any additional linker flags
   for the package.

But my hunch is that it would be much easier to just load up the 7meg
.so file.

``` haskell
ghc-8.4/libraries/ghc-prim/dist/build/cbits:
total 96K
drwxrwxr-x 2 chris chris 4.0K Feb 27 18:13 .
drwxr-xr-x 5 chris chris 4.0K May 25 11:54 ..
-rw-rw-r-- 1 chris chris 4.9K Feb 27 18:13 atomic.dyn_o
-rw-rw-r-- 1 chris chris 4.9K Feb 27 18:13 atomic.o
-rw-rw-r-- 1 chris chris 1.4K Feb 27 18:13 bswap.dyn_o
-rw-rw-r-- 1 chris chris 1.4K Feb 27 18:13 bswap.o
-rw-rw-r-- 1 chris chris 1.6K Feb 27 18:13 clz.dyn_o
-rw-rw-r-- 1 chris chris 1.6K Feb 27 18:13 clz.o
-rw-rw-r-- 1 chris chris 1.6K Feb 27 18:13 ctz.dyn_o
-rw-rw-r-- 1 chris chris 1.6K Feb 27 18:13 ctz.o
-rw-rw-r-- 1 chris chris 1.8K Feb 27 18:13 debug.dyn_o
-rw-rw-r-- 1 chris chris 1.8K Feb 27 18:13 debug.o
-rw-rw-r-- 1 chris chris  944 Feb 27 18:13 longlong.dyn_o
-rw-rw-r-- 1 chris chris  944 Feb 27 18:13 longlong.o
-rw-rw-r-- 1 chris chris 1.8K Feb 27 18:13 pdep.dyn_o
-rw-rw-r-- 1 chris chris 1.8K Feb 27 18:13 pdep.o
-rw-rw-r-- 1 chris chris 1.7K Feb 27 18:13 pext.dyn_o
-rw-rw-r-- 1 chris chris 1.7K Feb 27 18:13 pext.o
-rw-rw-r-- 1 chris chris 2.4K Feb 27 18:13 popcnt.dyn_o
-rw-rw-r-- 1 chris chris 2.4K Feb 27 18:13 popcnt.o
-rw-rw-r-- 1 chris chris 1.4K Feb 27 18:13 word2float.dyn_o
-rw-rw-r-- 1 chris chris 1.4K Feb 27 18:13 word2float.o
```

Demo for ghc-prim:

```
[6 of 8] Converting GHC.Debug
CCall (CCallSpec (StaticTarget NoSourceText "debugLn" (Just ghc-prim) True) CCallConv PlayRisky)
CCall (CCallSpec (StaticTarget NoSourceText "debugErrLn" (Just ghc-prim) True) CCallConv PlayRisky)
```

So:

```
chris@precision:~/Work/chrisdone/prana$ nm -g ghc-8.4/libraries/ghc-prim/dist/build/libHSghc-prim-0.5.2.0-Bfo9y0qb0emG5VRfx5d4mv-ghc8.4.3.so | grep debugLn
00000000003abd60 T debugLn
00000000003dd998 D ghczmprim_GHCziDebug_debugLn_closure
00000000003954c0 T ghczmprim_GHCziDebug_debugLn_info
chris@precision:~/Work/chrisdone/prana$ nm -g ghc-8.4/libraries/ghc-prim/dist/build/libHSghc-prim-0.5.2.0-Bfo9y0qb0emG5VRfx5d4mv-ghc8.4.3.so | grep debugErrLn
00000000003abd70 T debugErrLn
00000000003dd9a8 D ghczmprim_GHCziDebug_debugErrLn_closure
0000000000395568 T ghczmprim_GHCziDebug_debugErrLn_info
```

#### Parallel goals

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Boot process to compile the wired in packages| :construction: | Takes work |
|Analysis package `prana-analysis`| – | Fairly straight-forward |

I now need a reproducible way to build:

* `ghc-prim`
* `integer-simple`
* `base`

DON'T FORGET (2019 Oct 27):

When rebuilding these, run

    rm -rf /home/chris/.stack/programs/x86_64-linux/ghc-8.4.3/lib/ghc-8.4.3/prana/

to reset the package index.

ghc-prim

     $ stack build && stack ghc -- Setup.hs -v0 && stack exec --no-ghc-package-path -- ./Setup configure -v0 --with-ghc prana-ghc && stack exec --no-ghc-package-path ./Setup build

integer-gmp

     $ stack build && stack ghc -- Setup.hs -v0 && stack exec --no-ghc-package-path -- ./Setup configure -v0 --with-ghc prana-ghc && stack exec --no-ghc-package-path ./Setup build

base

https://github.com/valderman/haste-compiler/issues/11#issuecomment-184452053

    $ autoreconf
    $ stack build && stack ghc -- Setup.hs -v0 && stack exec --no-ghc-package-path -- ./Setup configure -finteger-gmp -v0 --with-ghc prana-ghc  --ghc-options=-O0 && stack exec --no-ghc-package-path -- ./Setup build --ghc-options=-O0


* custom ghc:
  https://github.com/commercialhaskell/stack/issues/725#issuecomment-364624897
  doesn't work
  https://github.com/commercialhaskell/stack/issues/725#issuecomment-496929378

* But this works via compiler-tools: https://github.com/commercialhaskell/stack/issues/725#issuecomment-496935164

Stack refuses to build core packages, so we'll have to put that in
prana-boot.

I need a check that:

1. Literally looks at the list of packages in scope.
2. Complains if any of those packages are not built by prana, ask the
   user to run prana-boot.

That should solve all my issues.

#### Optional goals

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Move from `binary` something faster [that also works](https://github.com/minad/persist/issues/4)| – | Takes yak shaving |
|Optimization of interpreter| – | Detailed |
|Allocation tests| – | Easy |
|CPU instruction tests| – | Easy |

#### Dream goals

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Distributed evaluation| – | Detailed, space for UX design |
|strace-like functionality for all primops| – | Pretty easy |
|Fuzzing of primops (think: threads, exceptions)| – | Fun, space for UX design |
|Prana web service| – | Straight-forward |
|JavaScript implementation| – | Takes some work |

#### Tooling goals

|Milestone|Status|Appraisal|
|:---|:---|:---|
|A web UI| – | Takes some work |
|Emacs integration| – | Takes some work |
|Your-IDE-here| – | ? |

## Primitive types

The following are planned to be implemented as standard:

|Milestone|Status|
|:---|:---|
|`Char#`|:heavy_check_mark:|
|`Int#`|:heavy_check_mark:|
|`Int8#`|-|
|`Word8#`|-|
|`Int16#`|-|
|`Word16#`|-|
|`Word#`|:heavy_check_mark:|
|`Int64#`|:heavy_check_mark:|
|`Word64#`|:heavy_check_mark:|
|`Double#`|:heavy_check_mark:|
|`Float#`|:heavy_check_mark:|
|`Array# a`|:heavy_check_mark:|
|`MutableArray# s a`|:heavy_check_mark:|
|`SmallArray# a`|:heavy_check_mark:|
|`SmallMutableArray# s a`|:heavy_check_mark:|
|`ByteArray#`|:heavy_check_mark:|
|`MutableByteArray# s`|:heavy_check_mark:|
|`ArrayArray#`|-|
|`MutableArrayArray# s`|-|
|`Addr#`|:heavy_check_mark:|
|`MutVar# s a`|-|
|`Weak# b`|-|
|`StablePtr# a`|-|
|`StableName# a`|-|

The following are needed for IO:

|Milestone|Status|
|:---|:---|
|`State# s`|-|
|`RealWorld`|-|
|`ThreadId#`|-|

The following are more exotic types to be implemented later:

|Milestone|Status|
|:---|:---|
|`TVar# s a`|-|
|`MVar# s a`|-|
|`Compact#`|-|

The following don't need implementing or won't be implemented:

* `BCO#`
* `Proxy# a`
* The SIMD vector operations

## Notable differences with GHCi

* Handles unboxed tuples as any other type.

## Workarounds and issues

Use of libffi may show this

    error while loading shared libraries: libffi.so.7: cannot open shared object file: No such file or directory

which is due to this: https://gitlab.haskell.org/ghc/ghc/issues/15397

Workaround, set the same path as your GHC's rts path:

    LD_LIBRARY_PATH=/home/chris/.stack/programs/x86_64-linux/ghc-8.4.3/lib/ghc-8.4.3/rts/

which has the libffi.so.7 in it.
