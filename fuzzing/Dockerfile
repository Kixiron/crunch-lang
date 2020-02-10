FROM rust:latest
ARG REPO

RUN rustup update
RUN git clone ${REPO}
RUN git checkout dev
RUN cd crunch-lang/fuzz
RUN sudo fuzz.sh
