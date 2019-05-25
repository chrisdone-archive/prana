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

#### Parallel goals

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Reproducible (Docker) build of GHC base packages| :construction: | Takes work |
|Analysis package `prana-analysis`| – | Fairly straight-forward |
|Make patched GHC for [standard location outputs](https://github.com/grin-tech/ghc-grin/blob/ea00b4ed18e2977dabb9c41ddcc28699ea96a85a/ghc-8.6.2.patch) | - | Straight-forward |

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
