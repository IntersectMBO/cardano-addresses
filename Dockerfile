# # Final image is based on scratch. We copy a bunch of Nix dependencies
# # but they're fully self-contained so we don't need Nix anymore.
FROM alpine:latest

WORKDIR /app

# Copy /nix/store
COPY ./tmp /nix/store
COPY ./tmp/cardano-address /app

RUN mkdir /etc/bash_completion.d
RUN /app/cardano-address --bash-completion-script `which /app/cardano-address` > /etc/bash_completion.d/cardano-address
RUN echo "source /etc/bash_completion.d/cardano-address" >> ~/.bashrc
RUN echo "/app/cardano-address --help" >> ~/.bashrc

ENTRYPOINT ["/app/cardano-address"]
