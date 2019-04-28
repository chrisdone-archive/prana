# prana

An interpreter for GHC STG written in Haskell itself

:construction: Prana is under construction.

## Milestones

Pure-Haskell goals:

|Milestone|Status|
|:---|:---|
|Clean STG from GHC|  :heavy_check_mark: |
|Interpreter prototype for AST| :heavy_check_mark: |
|Basic test suite for interpreter| :heavy_check_mark: |
|Move interpreter into `prana-interpret` library| :heavy_check_mark: |
|Get code coverage to near 100%| :construction: |
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
