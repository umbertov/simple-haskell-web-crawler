FROM haskell:8

ENV workdir /opt/crawler

WORKDIR $workdir

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./Crawler.cabal $workdir

# Add and Install Application Code
COPY . $workdir

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cd /opt/crawler && cabal install --only-dependencies -j4

RUN echo $PWD ; echo $(pwd)
RUN cabal build

