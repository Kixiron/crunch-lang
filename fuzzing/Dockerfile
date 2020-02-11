FROM rust:latest
ARG BRANCH=master

RUN rustup update
RUN git clone https://github.com/Kixiron/crunch-lang.git -b ${BRANCH}
RUN cd fuzzing
RUN sh ./fuzz.sh
