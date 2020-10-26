FROM haskell:8.8
COPY . /merger
WORKDIR /merger
RUN cabal update && cabal v2-install
ENTRYPOINT ~/.cabal/bin/merger-bot
