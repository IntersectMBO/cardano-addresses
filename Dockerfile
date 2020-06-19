#                                                                              #
# --------------------------------- BUILD ------------------------------------ #
#                                                                              #

FROM haskell:8.6.5 as build
WORKDIR /build
RUN apt-get update && apt-get install --no-install-recommends -y \
  build-essential=12.3 \
  git=1:2.11.*
RUN stack upgrade --binary-version 2.1.3

# Leverage Docker cache, build in three steps: 1) snapshot, 2) deps, 3) app
COPY stack.yaml .
SHELL ["/bin/bash", "-c"]
RUN echo $'name: placeholder \n\
library:                     \n\
  dependencies:              \n\
  - aeson                    \n\
  - base                     \n\
  - base-compat              \n\
  - bifunctors               \n\
  - bytestring               \n\
  - extra                    \n\
  - lens                     \n\
  - ordered-containers       \n\
  - text                     \n\
  - unordered-containers     \n\
  - vector                   \n\
' > package.yaml
RUN stack setup
RUN stack build --only-snapshot
RUN rm placeholder.cabal

COPY package.yaml .
RUN stack build --only-dependencies
COPY . .
RUN stack install --flag cardano-addresses:release

#                                                                              #
# ---------------------------------- RUN ------------------------------------- #
#                                                                              #

FROM frolvlad/alpine-glibc:alpine-3.11_glibc-2.30
RUN apk add --no-cache gmp=6.1.2-r1 bash=5.0.11-r1 bash-completion=2.9-r0
COPY --from=build /root/.local/bin /bin
RUN mkdir /etc/bash_completion.d
RUN cardano-address --bash-completion-script `which cardano-address` > /etc/bash_completion.d/cardano-address
RUN echo "source /etc/bash_completion.d/cardano-address" >> ~/.bashrc
RUN echo "cardano-address --help" >> ~/.bashrc
ENTRYPOINT ["cardano-address"]
