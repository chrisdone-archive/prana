# prana

An interpreter for GHC STG written in Haskell itself

:construction: Prana is under construction.

## Milestones

Pure-Haskell goals:

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Clean STG from GHC|  :heavy_check_mark: | Laborious |
|Interpreter prototype for AST| :heavy_check_mark: | Fairly easy |
|Basic test suite for interpreter| :heavy_check_mark: | Easy |
|Move interpreter into `prana-interpret` library| :heavy_check_mark: | Easy |
|Get code coverage to near 100%| :construction: | Detailed |
|Support all primitive types (`Char#` etc.)| – | Fairly straight-forward |
|Automated pure primops derivation| – | Fairly straight-forward |
|Move from `binary` to `persist`| – | Easy |

GHC Haskell goals:

|Milestone|Status|Appraisal|
|:---|:---|:---|
|Add hotswapping support| – | Requires exploration/experiment |
|IO primops| – | Fairly straight-forward |
|Runtime exceptions (`raise#`)| – | Tricky, space for UX design |
|FFI support| – | Requires learning but should design itself |
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
