set -e
set -x

stack build --test --no-run-tests

rm -r ~/Work/chrisdone/prana/prana-dir/
mkdir -p ~/Work/chrisdone/prana/prana-dir/packages/

cd ~/Work/chrisdone/prana/ghc-8.4/libraries/ghc-prim/

PRANA_DIR=~/Work/chrisdone/prana/prana-dir/ PRANA_MODE=INSTALL time -p -- ./Setup build --ghc-options="-O0"

cd ../integer-gmp/

PRANA_DIR=~/Work/chrisdone/prana/prana-dir/ PRANA_MODE=INSTALL time -p -- ./Setup build --ghc-options="-O0"

cd ../base-4.11.1.0/

PRANA_DIR=~/Work/chrisdone/prana/prana-dir/ PRANA_MODE=INSTALL time -p -- ./Setup build --ghc-options="-O0"
