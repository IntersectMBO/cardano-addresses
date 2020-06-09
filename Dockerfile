#                                                                              #
# --------------------------------- BUILD ------------------------------------ #
#                                                                              #

FROM haskell:8.6.5 as build
WORKDIR /build
RUN apt-get update && apt-get install --no-install-recommends -y \
  build-essential=12.3 \
  git=1:2.11.*
RUN stack upgrade --binary-version 2.1.3

COPY stack.yaml package.yaml ./
RUN stack setup
RUN stack build --only-snapshot
RUN stack build --only-dependencies

COPY . .
RUN stack install

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
ENTRYPOINT ["/root/cardano-address"]
