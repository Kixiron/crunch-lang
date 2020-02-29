FROM rust:latest

ENV DEV 1

VOLUME /crunch-docker
RUN cd /crunch-docker

# Update stuff
RUN apt update
RUN yes | apt --fix-broken install
RUN yes | apt upgrade
RUN rustup update

# honggfuzz
RUN apt install -y build-essential binutils-dev libunwind-dev libblocksruntime-dev
RUN cargo install honggfuzz

# flamegraph
RUN apt install -y linux-tools-common linux-tools-generic
RUN cargo install flamegraph

# Pull from github
RUN git pull https://github.com/Kixiron/crunch-lang
RUN if [$DEV == 1] then git checkout dev fi
