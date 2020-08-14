#!/bin/sh

ddlog -i ddlog/typecheck.dl -L .
exit_code=$?
if [ $exit_code -ne 0 ]; then
    exit $exit_code
fi

cd ddlog/typecheck_ddlog
cargo build
cd ../..
ddlog/typecheck_ddlog/target/debug/typecheck_cli < ddlog/typecheck.dat
