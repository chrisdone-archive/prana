set -e
set -x

# echo $PREFIX
# /home/chris/Work/chrisdone/prana/ghc-8.4/libraries/my-pkg-db

stack build --test --no-run-tests

rm -r ~/Work/chrisdone/prana/prana-dir/
mkdir -p ~/Work/chrisdone/prana/prana-dir/packages/

cd ~/Work/chrisdone/prana/ghc-8.4/libraries/ghc-prim/

PRANA_DIR=~/Work/chrisdone/prana/prana-dir/ PRANA_MODE=INSTALL time -p -- ./Setup build --ghc-options="-O0"

cd ../integer-gmp/

PRANA_DIR=~/Work/chrisdone/prana/prana-dir/ PRANA_MODE=INSTALL time -p -- ./Setup build --ghc-options="-O0"

cd ../base-4.11.1.0/

# Configuration was:
# ./Setup configure -finteger-gmp --package-db $PREFIX/package.conf.d --prefix $PREFIX --with-compiler $(stack exec which prana-ghc)

PRANA_DIR=~/Work/chrisdone/prana/prana-dir/ PRANA_MODE=INSTALL time -p -- ./Setup build --ghc-options="-O0"

cd ../array-0.5.2.0/

# ./Setup configure --package-db $PREFIX/package.conf.d --prefix $PREFIX --with-compiler $(stack exec which prana-ghc)

PRANA_DIR=~/Work/chrisdone/prana/prana-dir/ PRANA_MODE=INSTALL time -p -- ./Setup build --ghc-options="-O0"

cd ../deepseq-1.4.3.0/

PRANA_DIR=~/Work/chrisdone/prana/prana-dir/ PRANA_MODE=INSTALL time -p -- ./Setup build --ghc-options="-O0"

cd ../containers-0.5.11.0/

PRANA_DIR=~/Work/chrisdone/prana/prana-dir/ PRANA_MODE=INSTALL time -p -- ./Setup build --ghc-options="-O0"
