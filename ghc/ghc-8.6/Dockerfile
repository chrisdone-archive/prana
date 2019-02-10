FROM debian:stretch-slim
MAINTAINER Chris Done

ENV LANG     C.UTF-8
ENV LC_ALL   C.UTF-8
ENV LANGUAGE C.UTF-8

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

# Getting the GHC sources

RUN cd; \
    mkdir ghc_build; \
    cd ghc_build; \
    git clone -b ghc-8.6 --recursive git://git.haskell.org/ghc.git ghc-8.6; \
    cd ghc-8.6; \
    git checkout ghc-8.6; \
    git submodule update --init


# Building

RUN cd; \
    cd ghc_build/ghc-8.6; \
    cd mk; \
    sed -e 's/^#BuildFlavour = quick$/BuildFlavour = devel2/' \
        build.mk.sample > build.mk; \
    cd ..; \
    ./boot; \
    ./configure

RUN cd;cd ghc_build/ghc-8.6; make V=0 -j5 inplace/bin/ghc-stage2

# Patching
