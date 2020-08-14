#!/bin/sh

ddlog -i ddlog/typecheck.dl -L ddlog/modules --output-dir=.
exit_code=$?
if [ $exit_code -ne 0 ]; then
    exit $exit_code
fi

cd typecheck_ddlog
cargo build --bin typecheck_cli

target/debug/typecheck_cli < ../ddlog/typecheck.dat
