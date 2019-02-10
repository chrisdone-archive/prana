FROM debian:stretch-slim
MAINTAINER Chris Done

ENV LANG     C.UTF-8
ENV LC_ALL   C.UTF-8
ENV LANGUAGE C.UTF-8

################################################################################
# Preparation

RUN apt-get update; \
    apt-get -y install git autoconf automake libtool make gcc g++ \
                       libgmp-dev ncurses-dev libtinfo-dev python3 xz-utils

RUN apt-get install wget; \
    mkdir tmp.haskell-platform; \
    cd tmp.haskell-platform; \
    wget https://haskell.org/platform/download/8.4.3/haskell-platform-8.4.3-unknown-posix--core-x86_64.tar.gz; \
    tar xzf haskell-platform-8.4.3-unknown-posix--core-x86_64.tar.gz; \
    ./install-haskell-platform.sh; \
    cd ..; \
    rm -fr tmp.haskell-platform; \
    cd /usr/local/haskell/ghc-8.4.3-x86_64/lib/ghc-8.4.3; \
    sed -ie 's/^, ("C compiler supports -no-pie","NO")/, ("C compiler supports -no-pie","YES")/' settings

################################################################################
# Getting the GHC sources

RUN cd; \
    mkdir ghc_build; \
    cd ghc_build; \
    git clone -b ghc-8.6 --recursive git://git.haskell.org/ghc.git ghc-8.6; \
    cd ghc-8.6; \
    git checkout ghc-8.6; \
    git submodule update --init

################################################################################
# Building GHC

RUN cd; \
    cd ghc_build/ghc-8.6; \
    cd mk; \
    sed -e 's/^#BuildFlavour = quick$/BuildFlavour = quickest/' \
        build.mk.sample > build.mk; \
    cd ..; \
    ./boot; \
    ./configure

RUN cd;cd ghc_build/ghc-8.6; make V=0 -j5 inplace/bin/ghc-stage2

################################################################################
# Build primop generator binary

WORKDIR /root/ghc_build/ghc-8.6/utils/genprimopcode
RUN echo 'import Distribution.Simple; main = defaultMain' > Setup.hs && \
    runhaskell Setup.hs configure && \
    runhaskell Setup.hs build
RUN cp dist/build/genprimopcode/genprimopcode .

################################################################################
# Build base packages

# Setup an empty package database

ENV PREFIX /root/prana

RUN mkdir $PREFIX && \
    touch $PREFIX/names-cache.db && \
    mkdir -p $PREFIX/package.conf.d && \
    /root/ghc_build/ghc-8.6/inplace/bin/ghc-pkg --package-db $PREFIX/package.conf.d recache && \
    /root/ghc_build/ghc-8.6/inplace/bin/ghc-pkg --package-db $PREFIX/package.conf.d list

# Building ghc-prim

WORKDIR /root/ghc_build/ghc-8.6/libraries/ghc-prim

RUN cp /root/ghc_build/ghc-8.6/compiler/stage2/build/primops.txt ../../compiler/prelude/primops.txt && \
    ghc Setup.hs -v0  >/dev/null && \
    ./Setup configure -v0 --package-db $PREFIX/package.conf.d --prefix $PREFIX  --with-ghc /root/ghc_build/ghc-8.6/inplace/bin/ghc-stage2 >/dev/null && \
    ./Setup build -v --ghc-options="-O0" && \
    ./Setup install -v0 && \
    sed -i -e 's,^exposed-modules:,exposed-modules: GHC.Prim,' $PREFIX/package.conf.d/ghc-prim-*.conf && \
    /root/ghc_build/ghc-8.6/inplace/bin/ghc-pkg --package-db $PREFIX/package.conf.d recache

# Building integer-gmp

WORKDIR /root/ghc_build/ghc-8.6/libraries/integer-gmp
RUN ghc Setup.hs -v0  >/dev/null && \
    ./Setup configure -v0 --package-db $PREFIX/package.conf.d --prefix $PREFIX --with-ghc /root/ghc_build/ghc-8.6/inplace/bin/ghc-stage2  >/dev/null  && \
    ./Setup build -v0 --ghc-options="-O0" && \
    ./Setup install -v0  >/dev/null && \
    /root/ghc_build/ghc-8.6/inplace/bin/ghc-pkg --package-db $PREFIX/package.conf.d recache

# Building base

WORKDIR /root/ghc_build/ghc-8.6/libraries/base/
RUN echo 'import Distribution.Simple; main = defaultMainWithHooks autoconfUserHooks' > Setup.hs
RUN ghc -hide-all-packages -package Cabal Setup.hs -XNoImplicitPrelude && \
    ./Setup configure -v0 -finteger-gmp --package-db $PREFIX/package.conf.d --prefix $PREFIX   --with-ghc /root/ghc_build/ghc-8.6/inplace/bin/ghc-stage2  >/dev/null && \
    ./Setup build -v --ghc-options="-O0" && \
    ./Setup install -v0 >/dev/null
