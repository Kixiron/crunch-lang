FROM ubuntu:latest

ENV PATH=/usr/local/cargo/bin:$PATH \
    RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo

# Update stuff
RUN apt update
RUN yes | apt --fix-broken install
RUN yes | apt upgrade
RUN apt install -y curl git libssl-dev pkg-config
# hongfuzz
RUN apt install -y linux-tools-common linux-tools-generic
# flamegraph
RUN apt install -y build-essential binutils-dev libunwind-dev libblocksruntime-dev

# Install & Update Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
RUN rustup toolchain install nightly
RUN rustup update

RUN cargo install honggfuzz
RUN cargo install flamegraph
RUN cargo install cargo-make
