FROM rust:latest
ARG REPO

RUN rustup update
RUN git clone ${REPO}
RUN cd crunch-lang/fuzz
RUN sudo fuzz.sh
