# prana

An interpreter for GHC STG written in Haskell itself

:construction: Prana is under construction.

See
[test/assets/](https://github.com/chrisdone/prana/tree/master/prana-interpreter/test/assets)
under `prana-interpreter` for example programs that run with prana.

## Milestones

Pure-Haskell goals:

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Clean STG from GHC|  :heavy_check_mark: | Laborious |
|Interpreter prototype for AST| :heavy_check_mark: | Fairly easy |
|Basic test suite for interpreter| :heavy_check_mark: | Easy |
|Move interpreter into `prana-interpret` library| :heavy_check_mark: | Easy |
|Report unimplemented primops in base packages| – | Straight-forward |
|Support all primitive types (`Char#` etc.)| – | Fairly straight-forward |
|Automated pure primops derivation| :construction: | Straight-forward |
|Get code coverage to near 100%| :construction: | Detailed |
|Move from `binary` to `persist`| – | Easy |

GHC Haskell goals:

|Milestone|Status|Appraisal|
|:---|:---|:---|
|IO primops| – | Fairly straight-forward |
|Runtime exceptions (`raise#`)| – | Tricky, space for UX design |
|FFI support| – | Requires learning but should design itself |
|Run [base package tests](https://github.com/ghc/packages-base/tree/master/tests) | – | Fairly straight-forward |
|Add hotswapping support| – | Requires exploration/experiment |
|Threaded runtime/concurrency| – | Tricky |

Parallel goals:

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Reproducible (Docker) build of GHC base packages| :construction: | Takes work |
|Analysis package `prana-analysis`| – | Fairly straight-forward |

Optional goals:

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Optimization of interpreter| – | Detailed |
|Allocation tests| – | Easy |
|CPU instruction tests| – | Easy |

Dream goals:

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Prana web service| – | Straight-forward |
|JavaScript implementation| – | Takes some work |

## Primitive types

The following are planned to be implemented as standard:

|Milestone|Status|
|:---|:---|
|`Char#`|-|
|`Int#`|-|
|`Int8#`|-|
|`Word8#`|-|
|`Int16#`|-|
|`Word16#`|-|
|`Word#`|-|
|`Int64#`|-|
|`Word64#`|-|
|`Double#`|-|
|`Float#`|-|
|`Array# a`|-|
|`MutableArray# s a`|-|
|`SmallArray# a`|-|
|`SmallMutableArray# s a`|-|
|`ByteArray#`|-|
|`MutableByteArray# s`|-|
|`ArrayArray#`|-|
|`MutableArrayArray# s`|-|
|`Addr#`|-|
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
