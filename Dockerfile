FROM ubuntu:latest

ENV DEV 1

VOLUME /crunch-docker
RUN cd /crunch-docker

# Update stuff
RUN apt update
RUN yes | apt --fix-broken install
RUN yes | apt upgrade
RUN apt install -y curl
# hongfuzz
RUN apt install -y linux-tools-common linux-tools-generic
# flamegraph
RUN apt install -y build-essential binutils-dev libunwind-dev libblocksruntime-dev

# Install & Update Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
RUN PATH="$PATH:/usr/local/cargo/bin"
RUN rustup toolchain install nightly
RUN rustup update

RUN cargo install honggfuzz
RUN cargo install flamegraph

# Pull from github
RUN git pull https://github.com/Kixiron/crunch-lang
RUN if [$DEV == 1] ; then git checkout dev fi
