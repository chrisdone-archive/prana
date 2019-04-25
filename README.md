# prana

An interpreter for GHC STG written in Haskell itself

:construction: Prana is under construction.

## Milestones

Pure-Haskell goals:

|Milestone|Status|
|:---|:---|
|Clean STG from GHC|  :heavy_check_mark: |
|Interpreter prototype for AST| :construction: |
|Test suite for interpreter| – |
|Move interpreter into `prana-interpret` library| – |
|Get code coverage to near 100%| – |
|Automated pure primops derivation| – |
|Move from `binary` to `persist`| – |

GHC Haskell goals:

|Milestone|Status|
|:---|:---|
|Add hotswapping support| – |
|IO primops| – |
|Runtime exceptions (`raise#`)| – |
|FFI support| – |
|Threaded runtime/concurrency| – |

Parallel goals:

|Milestone|Status|
|:---|:---|
|Reproducible (Docker) build of GHC base packages| :construction: |
|Analysis package `prana-analysis`| – |

Optional goals:

|Milestone|Status|
|:---|:---|
|Optimization of interpreter| – |
|Allocation tests| – |
|CPU instruction tests| – |

Dream goals:

|Milestone|Status|
|:---|:---|
|Prana web service| – |
|JavaScript implementation| – |
