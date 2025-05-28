FROM ubuntu:focal

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install --yes curl gcc g++ git libz-dev make libgmp3-dev libtcl8.6 libtinfo-dev wget

ENV BSC_VER=2023.07
RUN mkdir -p /root/bsc
RUN wget https://github.com/B-Lang-org/bsc/releases/download/$BSC_VER/bsc-$BSC_VER-ubuntu-20.04.tar.gz -O /root/bsc.tar.gz && \
    tar -xvf /root/bsc.tar.gz -C /root/bsc --strip-components=1 && \
    rm /root/bsc.tar.gz

ENV GHCUP_VER=0.1.22.0
RUN mkdir -p /root/.ghcup/bin
RUN wget https://downloads.haskell.org/~ghcup/$GHCUP_VER/x86_64-linux-ghcup-$GHCUP_VER -O /root/.ghcup/bin/ghcup && \
    chmod a+x /root/.ghcup/bin/ghcup

ENV PATH=/root/bsc/bin:/root/.cabal/bin:/root/.ghcup/bin:$PATH

ENV GHC_VER=9.4.8
ENV CABAL_VER=3.8.1.0
RUN ghcup install ghc $GHC_VER && \
    ghcup set ghc $GHC_VER && \
    ghcup install cabal $CABAL_VER && \
    ghcup set cabal $CABAL_VER && \
    cabal update

COPY . /copilot-bluespec
WORKDIR /copilot-bluespec

RUN cabal configure --enable-tests && \
    cabal build --write-ghc-environment-files=always && \
    cabal install --lib copilot . --package-env . && \
    cabal test

ENTRYPOINT ["/usr/bin/bash"]
